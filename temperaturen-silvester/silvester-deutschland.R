library(tidyverse)
library(lubridate)
library(here)
library(rdwd)
library(ggtext)

base_path <- here("temperaturen-silvester")

# find all stations
findID("*", exactmatch = FALSE)

# select all links
links <- selectDWD("*", res = "daily", var = "kl", per = "hr",
          exactmatch = FALSE)

# select all stations with recent historical data
stations_hist <- links %>% 
  str_extract("\\d{5}_\\d{8}_\\d{8}") %>% 
  str_split_fixed(pattern = "_", n = 3) %>% 
  as.data.frame() %>% 
  rename(station_id = V1, start_date = V2, latest_date = V3) %>% 
  filter(latest_date > "2021") 

stations_recent <- links %>% 
  str_extract("\\d{5}_akt") %>% 
  str_extract("\\d{5}") %>% 
  as.data.frame() %>% 
  rename(station_id = 1) %>% 
  filter(!is.na(.))

stations_recent %>% 
  anti_join(stations_hist)

# links to eventually download
download_links <- tibble(link = links) %>% 
  mutate(station_id = str_extract(link, "\\d{5}")) %>% 
  semi_join(stations_recent, by = "station_id") %>% 
  arrange(station_id) %>% 
  group_by(station_id) %>% 
  filter(n() == 2) %>% 
  ungroup()


# Download all relevant files
files <- dataDWD(download_links$link, dir = here(base_path, "data"), read = FALSE, force = FALSE)
length(files)


# Read the file from the zip folder
read_climate_files <- function(file) {
  clim <- readDWD(file, varnames = TRUE) %>% bind_rows()
  colnames(clim) <- tolower(colnames(clim))
  clim
}

read_climate_files_possibly <- possibly(read_climate_files, otherwise = NULL)

climate_data <- map(files, read_climate_files_possibly)


link <- "daily/kl/historical/tageswerte_00699_19490101_19580630_hist.zip"
ind <- createIndex(link, dir=tempdir())
ind

# Retrieve and prepare station meta data ---------------------------------------

# Prepare station meta data
prepare_station_meta_data <- function(x) {
  x  %>% 
    filter(res == "daily", var == "kl") %>% 
    select(station_id = Stations_id, Stationsname, Bundesland, 
           von_datum, bis_datum, Stationshoehe, geoBreite, geoLaenge)
}

meta_station_info_raw <- map(unique(download_links$station_id), metaInfo)
meta_station_info <- map_dfr(meta_station_info_raw, prepare_station_meta_data)


# # recode dataset
# recode_data <- function(df) {
#   temp_prep <- df %>% 
#     # there is a certain overlap between the two data sources, remove those duplicates
#     group_by(mess_datum) %>% 
#     slice_head(n = 1) %>% 
#     ungroup() %>% 
#     transmute(
#       date = lubridate::as_date(as.character(mess_datum)),
#       tmk.lufttemperatur, txk.lufttemperatur_max, tnk.lufttemperatur_min
#     ) %>% 
#     na.omit() %>% 
#     mutate(month = month(date, label = TRUE),
#            year = year(date),
#            decade = year %/% 10 * 10)
# }

silvester_temp <- climate_data %>%
  compact() %>%
  bind_rows() %>% 
  tibble() %>% 
  filter(month(mess_datum) == 12, mday(mess_datum) == 31) %>% 
  filter(!is.na(txk.lufttemperatur_max)) %>% 
  select(station_id = stations_id, mess_datum, txk.lufttemperatur_max, 
         tnk.lufttemperatur_min, tmk.lufttemperatur) %>% 
  mutate(year = year(mess_datum)) %>% 
  inner_join(meta_station_info, by = "station_id") %>% 
  select(-c(von_datum, bis_datum)) %>% 
  distinct()

# keep only stations with an entry for 2022 and active since at least 1961
silvester_temp_filtered <- silvester_temp %>%
  group_by(station_id) %>% 
  mutate(max_year = max(year),
         min_year = min(year)) %>% 
  ungroup() %>%
  filter(max_year == max(year) & min_year <= 1961) # 2022
(n_stations <- length(unique(silvester_temp_filtered$station_id)))


# Wann wurde der Temperaturrekord erreicht?
silvester_temp_filtered %>% 
  arrange(station_id, year) %>% 
  group_by(station_id) %>% 
  slice_max(order_by = txk.lufttemperatur_max, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(station_id, Stationsname, year, txk.lufttemperatur_max)

# Plot bar chart with record age in a given year
plot_temp_record_age <- function(year, df = silvester_temp_filtered) {
  y <- year
  df %>% 
    filter(year <= y) %>% 
    arrange(station_id, year) %>% 
    group_by(station_id) %>% 
    slice_max(order_by = txk.lufttemperatur_max, n = 1, with_ties = FALSE) %>% 
    ungroup() %>% 
    select(station_id, Stationsname, year, txk.lufttemperatur_max) %>% 
    mutate(record_age = y - year) %>% 
    ggplot(aes(record_age)) +
    geom_bar(color = "white") +
    coord_cartesian(xlim = c(0, 125), ylim = c(0, 225)) +
    labs(
      title = y
    )
}

plot_temp_record_age_binned <- function(year, df = silvester_temp_filtered) {
  y <- year
  df %>% 
    filter(year <= y) %>% 
    arrange(station_id, year) %>% 
    group_by(station_id) %>% 
    slice_max(order_by = txk.lufttemperatur_max, n = 1, with_ties = FALSE) %>% 
    ungroup() %>% 
    select(station_id, Stationsname, year, txk.lufttemperatur_max) %>% 
    mutate(
      record_age = y - year,
      record_age_grp = case_when(
        record_age == 0 ~ "Same year",
        record_age == 1 ~ "Previous year",
        record_age <= 5 ~ "2-5 years ago",
        record_age <= 20 ~ "6-20 years ago",
        record_age <= 50 ~ "21-50 years ago",
        TRUE ~ "More than 50 years ago"
      ),
      record_age_grp = factor(record_age_grp,
                              levels = c(
                                "Same year",
                                "Previous year",
                                "2-5 years ago",
                                "6-20 years ago",
                                "21-50 years ago",
                                "More than 50 years ago"
                              ))
    ) %>% 
    ggplot(aes(record_age_grp)) +
    geom_bar(color = "white") +
    coord_flip(ylim = c(0, 225)) +
    labs(
      title = y
    )
}


# Stand 2022
plot_temp_record_age(2022)
# 2021
plot_temp_record_age(2021)
# 2020
plot_temp_record_age(2020)

# Stand 2022
plot_temp_record_age_binned(2022)
# 2021
plot_temp_record_age_binned(2021)
# 2020
plot_temp_record_age_binned(2020)


# Abweichungen vom (bisherigen) Rekordwert in 2022

silvester_temp_max_pre_2022 <- silvester_temp_filtered %>% 
  filter(year < 2022) %>% 
  arrange(station_id, year) %>% 
  group_by(station_id) %>% 
  slice_max(order_by = txk.lufttemperatur_max, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(station_id, txk.lufttemperatur_max)


color_palette <- c(
  rev(RColorBrewer::brewer.pal(6, "Reds")),
  RColorBrewer::brewer.pal(4, "Blues"))
breaks <- seq(-6, 6, 1)



silvester_temp_max_diff_2022 <- silvester_temp_filtered %>% 
  filter(year == max(year)) %>% 
  select(-c(tnk.lufttemperatur_min, tmk.lufttemperatur, min_year, max_year)) %>% 
  inner_join(silvester_temp_max_pre_2022, by = "station_id", suffix = c(".2022", ".before")) %>% 
  mutate(diff_to_prev_record = txk.lufttemperatur_max.2022 - txk.lufttemperatur_max.before) %>% 
  mutate(diff_to_prev_record_binned = cut(diff_to_prev_record, breaks = breaks))


silvester_temp_max_diff_2022 %>% 
  ggplot(aes(diff_to_prev_record)) +
  geom_histogram(
    aes(fill = diff_to_prev_record < -0.5),
    binwidth = 1, color = "white", show.legend = FALSE) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",", suffix = "°C")) +
  # scale_fill_manual(values = c("#f54281", "#4287f5")) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "Abweichung zur bisherigen Höchsttemperatur",
    y = "Anzahl Wetterstationen"
  ) +
  theme_minimal(base_family = "Avenir Book") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
 
silvester_temp_max_diff_2022 %>% 
  ggplot(aes(diff_to_prev_record_binned)) +
  geom_bar(
    aes(fill = diff_to_prev_record_binned),
    width = 0.85, show.legend = FALSE) +
  # scale_x_discrete(labels = scales::number_format(decimal.mark = ",", suffix = "°C")) +
  # scale_fill_manual(values = c("#f54281", "#4287f5")) +
  scale_fill_manual(values = rev(color_palette)) +
  labs(
    title = "An den meisten Orten fielen 2022 die Silvester-Rekorde",
    subtitle = "Abweichung der Höchsttemperaturen an Silvester 2022 zur jeweiligen
    bisherigen Höchsttemperatur an Silvestertagen (°C)",
    caption = sprintf(
    "Daten: Deutscher Wetterdienst CDC, %d Stationen, die seit mindestens 1961 berichten.<br>
    Visualisierung: Ansgar Wolsing", n_stations),
    x = "Abweichung zur bisherigen Höchsttemperatur (°C)",
    y = "Anzahl Wetterstationen"
    ) +
  theme_minimal(base_family = "Avenir Book", base_size = 10) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(color = "grey20"),
    plot.title = element_text(family = "Avenir Medium", color = "grey2"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "temperaturen-silvester-abweichungen-de.png"), width = 5, height = 4)
