library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(rdwd)

base_path <- here("mean-temperature-daily")

#' https://github.com/brry/rdwd
findID("Koeln", exactmatch = FALSE)
link <- selectDWD("Koeln-Bonn", res = "daily", var = "kl", per = "hr",
                  exactmatch = FALSE)
link <- selectDWD("Koeln-Stammheim", res = "daily", var = "kl", per = "hr",
                  exactmatch = FALSE)

# Download that dataset, returning the local storage file name:
file <- dataDWD(link, dir = here(base_path, "data"), read = FALSE)
file

# Read the file from the zip folder:
clim <- readDWD(file, varnames=TRUE) %>% bind_rows()
colnames(clim) <- tolower(colnames(clim))

clim %>% 
  count(mess_datum, sort = TRUE) %>% head()

clim %>% 
  count(year(mess_datum), sort = TRUE) %>% 
  head()

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
         decade = year %/% 10 * 10) %>% 
  filter(year > 1957)

temp_current <- temp_prep %>% 
  filter(year == 2022) %>%
  mutate(day = row_number()) 


# Daily averages in between 1991 and 2020
temp_normal <- temp_prep %>% 
  filter(year >= 1991 & year <= 2020) %>%  
  # exclude Feb 29th
  mutate(day_of_month = day(date)) %>%
  filter(!(month == "Feb" & day_of_month == 29)) %>%
  group_by(year) %>% 
  mutate(day = row_number()) %>% 
  ungroup()

# p_temp_normal <- temp_normal %>% 
#   ggplot(aes(day, tmk.lufttemperatur)) +
#   geom_smooth(method = "loess", span = 0.1, se = FALSE, col = "grey20")

# temp_prep %>% 
#     filter(year == 2022) %>%  
#     mutate(day = row_number()) %>% 
#   ggplot() +
#   geom_line(
#     data = layer_data(p_temp_normal, 1),
#     aes(x, y),
#     size = 0.8) 

smoothed_daily_values <- loess(
  tmk.lufttemperatur ~ day, data = temp_normal, span = 0.1) %>% 
  predict()
temp_normal_smoothed <- temp_normal %>% 
  bind_cols(temp_normal_smoothed = smoothed_daily_values) %>% 
  distinct(day, temp_normal_smoothed)
nrow(temp_normal_smoothed)

smoothed_daily_values_current <- loess(
  tmk.lufttemperatur ~ day, data = temp_current, span = 2/52) %>% 
  predict()
temp_current_smoothed <- temp_current %>% 
  bind_cols(temp_smoothed = smoothed_daily_values_current) 
nrow(temp_current_smoothed)

temp_normal_smoothed %>% 
  ggplot(aes(day, temp_normal_smoothed)) +
  geom_line()
temp_current_smoothed %>% 
  ggplot(aes(day, temp_smoothed)) +
  geom_line()

month_breaks <- c(
  1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
)
names(month_breaks) <- month.abb

set.seed(4711)
temp_current_smoothed %>% 
  right_join(temp_normal_smoothed, by = "day") %>% 
  mutate(diff_from_normal = temp_smoothed - temp_normal_smoothed) %>% 
  ggplot(aes(x = day)) +
  annotate(
    "rect",
    xmin = month_breaks, xmax = c(month_breaks[2:12], 365),
    ymin = -Inf, ymax = Inf, fill = rep(c("grey92", "grey97"), 6)
  ) +
  geom_hline(
    data = data.frame(yintercept = seq(5, 25, 5), size = c(0.1, 0.2, 0.1, 0.2, 0.1)), 
    aes(yintercept = yintercept, size = size), color = "grey80"
  ) +
  geom_ribbon(
    aes(ymin = temp_normal_smoothed, 
        ymax = pmin(temp_normal_smoothed, temp_smoothed), fill = "colder"),
    color = "grey40", size = 0.2, alpha = 0.8) +
  geom_ribbon(
    aes(ymin = temp_smoothed, 
        ymax = pmin(temp_normal_smoothed, temp_smoothed), fill = "warmer"),
    color = "grey40", size = 0.2, alpha = 0.8) +
  geom_line(
    aes(day, temp_normal_smoothed), size = 0.8, color = "grey20"
  ) +
  ggforce::geom_mark_circle(
    data = ~group_by(., month) %>%  
      slice_max(order_by = abs(diff_from_normal), n = 1) %>% 
      ungroup() %>% 
      slice_max(order_by = abs(diff_from_normal), n = 6),
    aes(y = temp_smoothed, 
        label = paste0(
          ifelse(diff_from_normal > 0, "+", "-"),
          round(diff_from_normal, 1), "°C"), group = day),
    size = 0.3, con.size = 0.3, label.buffer	= unit(5, "mm"), expand = unit(2, "mm"),
    label.fontsize = 8, label.fill = alpha("white", 0.5)
  ) +
  annotate(
    "text", x = 365, y = 3.5, label = "normal 1991-2020", hjust = 1,
    family = "Roboto Condensed", size = 3, fontface = "bold"
  ) +
  scale_x_continuous(breaks = month_breaks, labels = month.abb, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(5, 25, 5)) +
  scale_fill_manual(values = c("colder" = "#5F7EB6", "warmer" = "#B95C5B")) +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  guides(fill = "none") +
  labs(
    title = "Mean temperature in Cologne, Germany 2022",
    subtitle = "Station Cologne-Stammheim. Baseline: 1991-2020, smoothed values",
    caption = "Smoothed with lowess (bw=2/52). Data: DWD Open Data.
    Visualisation: Ansgar Wolsing (adapted from Dominic Royé)",
    x = NULL, y = "°C"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    axis.title.y.left = element_text(angle = 0),
    text = element_text(color = "grey20"),
    plot.title = element_text(face = "bold", color = "black"),
    plot.title.position = "plot",
    plot.caption = element_markdown()
  )
ggsave(here(base_path, "cgn.png"), width = 7, height = 5)

