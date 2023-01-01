library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(rdwd)

base_path <- here("temperaturen-silvester")

links <- list()

findID("Koeln", exactmatch = FALSE)
links[["Köln"]] <- selectDWD("Koeln-Stammheim", res = "daily", var = "kl", per = "hr",
                  exactmatch = TRUE)

findID("Berlin", exactmatch = FALSE)
links[["Berlin"]] <- selectDWD("Berlin-Dahlem (FU)", res = "daily", var = "kl", per = "hr",
                  exactmatch = TRUE)

findID("Hamburg", exactmatch = FALSE)
links[["Hamburg"]] <- selectDWD("Hamburg-Fuhlsbuettel", res = "daily", var = "kl", per = "hr",
                  exactmatch = TRUE)

findID("München", exactmatch = FALSE)
links[["München"]] <- selectDWD("Muenchen-Stadt", res = "daily", var = "kl", per = "hr",
                  exactmatch = TRUE)


# Download datasets, returning the local storage file name
files <- map(links, ~dataDWD(.x, dir = here(base_path, "data"), read = FALSE, force = TRUE))
files <- set_names(files, names(links))
files

# Read the file from the zip folder
read_climate_files <- function(file) {
  clim <- readDWD(file, varnames = TRUE) %>% bind_rows()
  colnames(clim) <- tolower(colnames(clim))
  clim
}

# climate_data <- map(files, read_climate_files) %>% bind_rows(.id = "location")
climate_data <- map(files, read_climate_files)


# recode dataset 
recode_data <- function(df) {
  temp_prep <- df %>% 
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
           decade = year %/% 10 * 10)
}

climate_data_recoded <- map_dfr(climate_data, recode_data, .id = "location")

# Earliest available year per location
climate_data_recoded %>% 
  group_by(location) %>% 
  summarize(min(year))

# latest date per location
climate_data_recoded %>% 
  group_by(location) %>% 
  summarize(max(date))


#' Predictions:
#' Berlin: 15 (https://kachelmannwetter.com/de/wetter/2950159-berlin)
#' Hamburg: 14 (https://kachelmannwetter.com/de/wetter/2911298-hamburg)
#' München: 17 (https://kachelmannwetter.com/de/wetter/2867714-muenchen)
#' Köln: 17 (https://kachelmannwetter.com/de/wetter/2829575-stammheim)
predictions <- data.frame(location = c("Berlin", "Hamburg", "Köln", "München"),
                          max_temp_pred = c(15, 14, 17, 16),
                          since_2001 = "Vorhersage 2022")

p <- climate_data_recoded %>% 
  filter(month == "Dec", mday(date) == 31) %>% 
  filter(year < 2022) %>% 
  mutate(since_2001 = ifelse(year >= 2001, "Ab 2001", "Vor 2001"),
         since_2001 = factor(since_2001, levels = c("Vor 2001", "Ab 2001", "Vorhersage 2022"))) %>% 
  ggplot(aes(location, txk.lufttemperatur_max)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = since_2001, color = since_2001),
    shape = 21, width = 0.25
  ) +
  geom_point(
    data = predictions,
    aes(y = max_temp_pred, fill = "Vorhersage 2022", col = "Vorhersage 2022"),
    size = 2, shape = 21, stroke = 1
  ) +
  ggrepel::geom_text_repel(
    data = filter(predictions, location == "Berlin"),
    aes(y = max_temp_pred, label = "Vorhersage"),
    col = "#540D6E", size = 2.5, alpha = 0.9, hjust = 0, # nudge_x = 0.1,
    min.segment.length = 0, segment.size = 0.33, family = "Roboto Condensed",
    fontface = "bold"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  scale_fill_manual(values = c(
    "Vor 2001" = "grey72", "Ab 2001" ="grey40", "Vorhersage 2022" = alpha("#540D6E", 0.2))) +
  scale_color_manual(values = c("white", "white", "#540D6E")) +
  guides(
    fill = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "Temperatur-Rekorde an Silvester 2022?",
    subtitle = "Historische Tageshöchsttemperaturen am 31.12.
    sowie Vorhersagen für den 31.12.2022",
    caption = "Wetterstationen: Berlin-Dahlem (FU) (seit 1950), 
    Hamburg-Fuhlsbüttel (1936),
    Köln-Stammheim (1945), München-Stadt (1954).
    <br>
    Historische Daten: Deutscher Wetterdienst, 
    Vorhersage 2022: kachelmannwetter.com (31.12.2022 10:15 Uhr). 
    Visualisierung: Ansgar Wolsing",
    x = NULL,
    y = "Höchsttemperatur",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 9) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    text = element_text(color = "grey36", lineheight = 1.1),
    plot.title = element_text(color = "grey2", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(t = 2, b = 8)),
    plot.caption = element_markdown(hjust = 0, size = 6),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank(),
    axis.text.x.top = element_text(face = "bold")
  )
ggsave(here(base_path, "temperaturen-silvester-2022-w-predictions.png"), width = 5, height = 4)



p <- climate_data_recoded %>% 
  filter(month == "Dec", mday(date) == 31) %>%
  mutate(since_2001 = case_when(
    year < 2001 ~ "Vor 2001",
    year < 2022 ~ "Ab 2001",
    year == 2022 ~ "2022"
    ),
    since_2001 = factor(since_2001, levels = c("Vor 2001", "Ab 2001", "2022"))) %>% 
  ggplot(aes(location, txk.lufttemperatur_max)) +
  ggbeeswarm::geom_quasirandom(
    data = ~subset(., year < 2022),
    aes(fill = since_2001, color = since_2001),
    shape = 21, width = 0.25
  ) +
  geom_point(
    data = ~subset(., year == 2022),
    aes(fill = since_2001, col = since_2001),
    size = 2, shape = 21, stroke = 1
  ) +
  ggrepel::geom_text_repel(
    data = ~subset(., year == 2022 & location == "Berlin"),
    aes(label = since_2001),
    col = "#540D6E", size = 2.5, alpha = 0.9, hjust = 0, # nudge_x = 0.1,
    min.segment.length = 0, segment.size = 0.33, family = "Roboto Condensed",
    fontface = "bold"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  scale_fill_manual(values = c(
    "Vor 2001" = "grey72", "Ab 2001" ="grey40", "2022" = alpha("#540D6E", 0.2))) +
  scale_color_manual(values = c("Vor 2001" = "white", "Ab 2001" = "white", "2022" = "#540D6E")) +
  guides(
    fill = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "Temperatur-Rekorde an Silvester 2022",
    subtitle = "Historische Tageshöchsttemperaturen am 31.12.",
    caption = "Wetterstationen: Berlin-Dahlem (FU) (seit 1950), 
    Hamburg-Fuhlsbüttel (1936),
    Köln-Stammheim (1945), München-Stadt (1954).
    <br>
    Daten: Deutscher Wetterdienst. 
    Visualisierung: Ansgar Wolsing",
    x = NULL,
    y = "Höchsttemperatur",
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 9) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    text = element_text(color = "grey36", lineheight = 1.1),
    plot.title = element_text(color = "grey2", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(t = 2, b = 8)),
    plot.caption = element_markdown(hjust = 0, size = 6),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank(),
    axis.text.x.top = element_text(face = "bold")
  )
ggsave(here(base_path, "temperaturen-silvester-2022-messdaten.png"), width = 5, height = 4)


climate_data_recoded %>% 
  filter(month == "Dec", mday(date) == 31) %>% View()
