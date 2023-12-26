library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
#' https://github.com/brry/rdwd
# rdwd::updateRdwd()
library(rdwd)

base_path <- "snow-de"

# Index with metadata about all stations
data(metaIndex)
glimpse(metaIndex)

table(metaIndex$var)

# Select stations which have been active at least since min_date and are currently active
min_date <- as_date("1961-01-01")
active_stations_meta <- metaIndex %>%
  filter(von_datum <= min_date & var == "kl" & res == "monthly"
         & per == "recent" & hasfile)
n_active_stations <- nrow(active_stations_meta)


# Get the download URLs for all stations that match the criteria
station_ids <- unique(active_stations_meta$Stations_id)

# Update only recent files // run if you have pulled the data at least once before
update_only_recent <- TRUE
if (update_only_recent) {
  message("Updating recent files.")
  data_urls <- map(
    station_ids,
    ~selectDWD(id = .x, res = "daily", var = "kl",
               per = "r", # only recent
               exactmatch = TRUE))
} else {
  data_urls <- map(
    station_ids,
    ~selectDWD(id = .x, res = "daily", var = "kl", per = "hr",
               exactmatch = TRUE))
  message("Updating historic and recent files.")
}

# Download datasets, returning the local storage file name
files <- map(
  data_urls,
  ~dataDWD(.x, dir = file.path(base_path, "data", "dwd-stations"), read = FALSE, force = TRUE))

# Add the filepath of historical file (required if you only update the recent files)
files_hist_and_recent <- vector("list", length = length(files))
files_local_hist <- here(base_path, "data", "dwd-stations",
                         list.files(here(base_path, "data", "dwd-stations"),
                                    pattern = "daily_kl_historical"))
files_hist_and_recent <- map2(files, seq_along(files), function(x, i) {
  # find the historical data for this station
  station_id <- str_extract(x, "_(\\d{5})_akt.zip", group = 1)
  file_hist <- files_local_hist[str_detect(files_local_hist, sprintf("tageswerte_KL_%s_", station_id))]

  c(x, file_hist)
})

# Read the files from the zip archives
dfs <- map(files_hist_and_recent, readDWD, varnames = TRUE)

combine_historical_and_recent <- function(x) {
  df <- bind_rows(x)
  colnames(df) <- tolower(colnames(df))
  df <- df %>%
    transmute(
      stations_id,
      date = lubridate::as_date(as.character(mess_datum)),
      year = year(date),
      decade = (year - 1) %/% 10 * 10,
      shk_tag.schneehoehe
    ) %>%
    # na.omit() %>%
    filter(month(date) == 12 & mday(date) %in% 24:26) %>% 
    # there is a certain overlap between the two data sources,
    # remove those duplicates
    group_by(date) %>%
    slice_head(n = 1) %>%
    ungroup() 
  df
}

# Combine recent and historical data - this takes a couple of minutes
dfs_combined <- map(dfs, combine_historical_and_recent, .progress = TRUE)
dfs_combined <- set_names(dfs_combined, unique(active_stations_meta$Stationsname))
write_rds(dfs_combined, here(base_path, "output", "dwd-stations-pre.rds"))


# Add metadata to weather data
add_station_metadata <- function(x, metadata = active_stations_meta) {
  x %>%
    inner_join(metadata, by = join_by(stations_id == Stations_id)) %>%
    select(-c(res, var, per, hasfile))
}

dfs_prep <- map(dfs_combined, add_station_metadata, .progress = TRUE)
glimpse(dfs_prep[[1]])

df_snow_xmas <- dfs_prep %>% 
  bind_rows(.id = "stations_name") %>% 
  filter(year >= 1961) %>% 
  group_by(stations_name, stations_id, decade, year, geoBreite, geoLaenge) %>% 
  summarize(
    snow_xmas_max = max(shk_tag.schneehoehe),
    has_snow_xmas = snow_xmas_max > 0,
    has_snow_xmas_5cm = snow_xmas_max >= 5
  ) 

shp <- giscoR::gisco_get_countries(country = "Germany", resolution = "60")

df_snow_xmas %>% 
  filter(has_snow_xmas) %>% 
  ggplot(aes(geoLaenge, geoBreite, fill = snow_xmas_max)) +
  geom_sf(
    data = shp,
    fill = "grey92", color = "grey92", inherit.aes = FALSE
  ) +
  geom_point(
    shape = 21, color = "grey20", stroke = 0.1
  ) +
  scale_fill_gradient(
    low = "white", high = "blue", trans = "pseudo_log",
    breaks = c(3, 10, 30, 100, 300, 1000)) + 
  facet_wrap(vars(year), ncol = 10) +
  theme_void()


darkgreen_to_white_pal <- colorRampPalette(c("#718373", "white"))

p <- df_snow_xmas %>% 
  filter(has_snow_xmas) %>% 
  # exclude 2023
  filter(year < 2023) %>% 
  mutate(
    snow_xmas_max_grp = case_when(
      snow_xmas_max <= 5 ~ "0.1-5 cm",
      snow_xmas_max <= 10 ~ "6-10 cm",
      snow_xmas_max <= 30 ~ "11-30 cm",
      snow_xmas_max <= 50 ~ "31-50 cm",
      TRUE ~ "> 50 cm"
    ),
    snow_xmas_max_grp = factor(
      snow_xmas_max_grp,
      levels = c("0.1-5 cm", "6-10 cm",  "11-30 cm", "31-50 cm", "> 50 cm"))
  ) %>% 
  ggplot(aes(geoLaenge, geoBreite, fill = snow_xmas_max_grp)) +
  geom_sf(
    data = shp,
    fill = "#1D3A20", color = "#1D3A20", inherit.aes = FALSE
  ) +
  geom_point(
    shape = 21, color = "grey90", stroke = 0.1, size = 0.8
  ) +
  scale_fill_manual(values = darkgreen_to_white_pal(5)) +
  facet_wrap(vars(year), ncol = 10) +
  guides(
    fill = guide_legend(
      title = "Schneehöhe in cm (Maximum)",
      # title = "Snow depth in cm (max.)", 
      title.position = "top", title.hjust = 0.5, nrow = 1,
      override.aes = list(shape = 21, size = 4, color = "grey30"))
  ) +
  labs(
    title = "Schnee an Weihnachten",
    # title = "Dreaming of a White Christmas",
    # subtitle = "Combined snow depth from December 24 to December 26<br>
    # for 268 weather stations (locations constant over time).",
    subtitle = "Maximale Schneehöhe vom 24.12. bis 26.12. pro Jahr
    an 268 Wetterstationen (konstant über Zeit)",
    # caption = "<span style='font-family:Outfit Semibold'>Data:</span> DWD Open Data.
    # <span style='font-family:Outfit Semibold'>Visualization:</span> Ansgar Wolsing"
    caption = "<span style='font-family:Outfit Semibold'>Daten:</span> DWD Offene Daten.
    <span style='font-family:Outfit Semibold'>Visualisierung:</span> Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit Light", base_size = 10) +
  theme(
    plot.background = element_rect(color = "grey95", fill = "grey95"),
    plot.margin = margin(rep(4, 4)),
    plot.title = element_markdown(
      family = "Pinyon Script", size = 28, hjust = 0.5, color = "#A2231D"),
    plot.subtitle = element_textbox(
      width = 0.95, hjust = 0.5, halign = 0.5, lineheight = 1.1,
      margin = margin(t = 2, b = 12)),
    plot.caption = element_markdown(hjust = 0.5),
    strip.text = element_text(
      hjust = 0.5, family = "Outfit", size = 6,
      margin = margin(t = 2, b = 1)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.key.width = unit(5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.text = element_text(hjust = 0.5),
    legend.text.align = 0.5
  )
ggsave(here(base_path, "plots", "snow-xmas-de-de.png"), 
       dpi = 500, width = 4.25, height = 5.25, 
       scale = 1.2)



stations_names_to_show <- c(
  "Berlin-Dahlem (FU)", "Koeln/Bonn", "Muenchen-Stadt", "Hamburg-Fuhlsbuettel",
  "Stuttgart-Echterdingen", "Frankfurt/Main")

df_snow_xmas %>% 
  filter(stations_name %in% stations_names_to_show, decade <= 2010) %>% 
  count(decade, wt = has_snow_xmas, name = "n_xmas_snow") %>% 
  ggplot(aes(decade, n_xmas_snow)) +
  geom_col(
    aes(y = 1), fill = "grey60"
  ) +
  geom_col() +
  facet_wrap(vars(stations_name))

df_snow_xmas %>% 
  filter(stations_name %in% stations_names_to_show, decade <= 2010) %>% 
  count(decade, wt = has_snow_xmas_5cm, name = "n_xmas_snow") %>% 
  ggplot(aes(decade, n_xmas_snow)) +
  geom_col(
    aes(y = 1), fill = "grey60"
  ) +
  geom_col() +
  facet_wrap(vars(stations_name))



df_snow_xmas %>% 
  filter(stations_name %in% stations_names_to_show, decade <= 2010) %>% 
  group_by(stations_name, decade) %>% 
  summarize(avg_snow_xmas = mean(snow_xmas_total)) %>% 
  ggplot(aes(decade, avg_snow_xmas)) +
  geom_col() +
  facet_wrap(vars(stations_name))


