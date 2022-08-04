library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(rdwd)

base_path <- here("heat-days-de")

#' https://github.com/brry/rdwd
findID("Koeln", exactmatch = FALSE)
link <- selectDWD("Koeln-Bonn", res = "daily", var = "kl", per = "hr",
                  exactmatch = FALSE)

# Actually download that dataset, returning the local storage file name:
file <- dataDWD(link, dir = here(base_path, "data"), read = FALSE)
file

# Read the file from the zip folder:
clim <- readDWD(file, varnames=TRUE) %>% bind_rows()
colnames(clim) <- tolower(colnames(clim))

clim %>% 
  count(mess_datum, sort = TRUE) %>% head()



# recode dataset
temp_prep <- clim %>% 
  # there is a certain overlap between the two data sources, remove those duplicates
  group_by(mess_datum) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  transmute(
    date = lubridate::as_date(as.character(mess_datum)),
    tmk.lufttemperatur, txk.lufttemperatur_max, tnk.lufttemperatur_min
  ) %>% 
  na.omit() %>% 
  mutate(month = month(date, label = TRUE),
         year = year(date),
         decade = (year - 1) %/% 10 * 10) %>% 
  filter(year > 1957)

temp_prep %>% filter(year == 2021)
temp_prep %>% count(year) %>% tail()

# Heat days (>= 30°C) in 2022
temp_prep %>% 
  filter(year == 2022, txk.lufttemperatur_max >= 30) 

temp_prep %>% 
  filter(txk.lufttemperatur_max >= 30) %>% 
  count(year, sort = TRUE)


# All heat days since 1958
heat_days <- temp_prep %>% 
  filter(txk.lufttemperatur_max >= 30) 

# Count the days between the first of the year and the completion of n heat days
heat_days_threshold <- 14
years <- min(temp_prep$year) : max(temp_prep$year)
selected_dates <- as_date(paste0(years, "-06-01"))
number_of_days <- vector("integer", length(selected_dates))
for (i in seq_along(selected_dates)) {
  selected_date <- selected_dates[i]
  number_of_days[i] <- heat_days[heat_days$date >= selected_date, ] %>% 
    head(heat_days_threshold) %>% 
    tail(1) %>% 
    transmute(days_elapsed = date - selected_date) %>% 
    pull(days_elapsed)
}

days_heat <- tibble(
  date = selected_dates,
  year = year(selected_dates),
  days_to_heat = as.numeric(number_of_days)
)


days_heat %>% 
  mutate(decade = (year - 1) %/% 10 * 10) %>% 
  filter(year >= 1961 & year <= 2020) %>% 
  group_by(decade) %>% 
  mutate(days_to_heat_decade_mean = mean(days_to_heat),
         days_to_heat_decade_median = median(days_to_heat)) %>% 
  ungroup() %>% 
  ggplot(aes(year, days_to_heat)) + 
  geom_col(aes(fill = days_to_heat), width = 0.5) +
  # Annotation with the median
  # geom_richtext(
  #   data = ~subset(., year %% 10 == 4),
  #   aes(label = paste(
  #     ifelse(decade == 1960, "<span style='font-size:8pt'>Median</span><br>", ""),
  #     round(days_to_heat_decade_median), 
  #     "Tage"), 
  #     y = 900),
  #   fill = NA, label.size = 0, stat = "unique", family = "Libre Franklin SemiBold", 
  #   size = 4, color = "grey50") +
  # custom y-scale
  geom_richtext(
    data = data.frame(
      decade = 1960, x = 1959, y = seq(365, 4 * 365, 365),
      label = c("1", "2", "3", "4 Jahre")
    ),
    aes(label = label, x = x, y = y),
    fill = NA, label.size = 0, stat = "unique", family = "Libre Franklin", 
    size = 3, color = "grey50", vjust = 0) +
  scale_x_reverse() +
  scale_y_continuous(breaks = seq(365, 10 * 365, 365)) +
  scale_fill_gradient(high = "#edd12f", low = "#ed784a") +
  coord_flip(clip = "off") +
  facet_wrap(vars(decade), ncol = 1, strip.position = "right", scales = "free_y",
             labeller = as_labeller(function(x) 
               paste0(x, "er<br><span style='font-size:8pt;color:grey60'>Jahre</span>"))) +
  labs(
    title = "14 heiße Tage in Köln bis Anfang August 2022",
    subtitle = glue::glue("Anzahl der Tage, die es ab dem meteorologischen
    Sommeranfang (01.06.) jedes Jahres bis zum Erreichen von **{heat_days_threshold} Hitzetagen**
    (>= 30°C) dauerte"),
    caption = "Wetterstation Köln/Bonn Flughafen. Quelle: DWD CDC. Visualisierung: Ansgar Wolsing",
    fill = "Anzahl Kalendertage") +
  theme_void(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = NA, fill = "#ebf6f7"), #grey80
    legend.position = "bottom", 
    legend.key.height = unit(2, "mm"),
    plot.margin = margin(6, 12, 6, 6),
    panel.spacing.y = unit(8, "mm"),
    strip.text = element_markdown(color = "grey40", size = 14, hjust = 0, 
                                  lineheight = 0.1, vjust = 0.5, face = "bold"),
    panel.grid.major.x = element_line(color = "grey50", size = 0.2),
    plot.title = element_text(family = "Libre Franklin SemiBold"),
    plot.subtitle = element_textbox(width = 1, margin = margin(t = 8, b = 24),
                                    lineheight = 1.25))
ggsave(here(base_path, "plots", "cologne-heat-days.png"), width = 7, height = 8,
       dpi = 400)

