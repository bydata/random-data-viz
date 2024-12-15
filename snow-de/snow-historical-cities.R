library(tidyverse)
library(ggtext)
library(here)
#' https://github.com/brry/rdwd
rdwd::updateRdwd()
library(rdwd)

base_path <- "snow-de"


## Data Preparation ------------------------------------------------------------

# Index with metadata about all stations
data(metaIndex)
glimpse(metaIndex)

biggest_cities <- c("Berlin", "Hamburg", "München", "Köln", "Frankfurt/M",
                     "Stuttgart")

# Replace umlauts with ".e"
replace_umlauts <- function(x) {
  x |> 
    str_replace_all("ä", "ae") |> 
    str_replace_all("ö", "oe") |> 
    str_replace_all("ü", "ue")
}

biggest_cities_fmt <- replace_umlauts(biggest_cities)
names(biggest_cities) <- biggest_cities_fmt
biggest_cities_regex <- sprintf("^(%s)", paste(biggest_cities_fmt, collapse = "|"))

df_stations <- metaIndex |> 
  filter(res == "daily", var == "more_precip", hasfile) |> 
  filter(str_detect(Stationsname, biggest_cities_regex)) |> 
  filter(Stationsname != "Berlinchen") |> 
  mutate(
    city = str_extract(Stationsname, biggest_cities_regex),
    city = biggest_cities[city]) |> 
  distinct(Stations_id, city, Stationsname, von_datum, bis_datum) |> 
  arrange(city, von_datum, bis_datum)

# Get the download URLs for all stations that match the criteria
station_ids <- unique(df_stations$Stations_id)
data_urls <- map(
  station_ids,
  function(x) selectDWD(id = x, res = "daily", var = "more_precip", per = "hr",
                        exactmatch = TRUE))

# Download datasets, returning the local storage file name
files <- map(
  data_urls,
  function(x) dataDWD(x, dir = file.path(base_path, "data", "dwd-stations"), 
           read = FALSE, force = FALSE))

# Read the files from the zip archives
dfs <- map(files, readDWD, varnames = TRUE)
dfs <- set_names(dfs, df_stations$Stationsname)

#' Combine the dataframes, and identify years with snow at at least one weather 
#' station in a city the x-mas time
df_cities_snow <- dfs |> 
  # first call to bind_rows() to combine datasets with historical and recent data,
  # i.e. 2 list elements per station
  map(bind_rows) |> 
  bind_rows(.id = "stations_name") |> 
  janitor::clean_names() |> 
  inner_join(df_stations, by = join_by(stations_id == Stations_id)) |> 
  filter(month(mess_datum) == 12, mday(mess_datum) %in% 24:26) |> 
  mutate(year = year(mess_datum)) |> 
  distinct(city, year, stations_name = Stationsname, stations_id, 
           mess_datum, sh_tag_schneehoehe) |>
  rename(snow_cm = sh_tag_schneehoehe) |> 
  mutate(
    snow_cm = replace_na(snow_cm, 0),
    snow_1cm = snow_cm >= 1,
    snow_2cm = snow_cm >= 2,
    snow_5cm = snow_cm >= 5) |> 
  group_by(city, year) |>
  summarize(
    snow_1cm = max(snow_1cm) == 1,
    snow_2cm = max(snow_2cm) == 1,
    snow_5cm = max(snow_5cm) == 1, 
    .groups = "drop")

# Identify the min year across all stations
(first_year_overall <- df_cities_snow |> 
  group_by(city) |> 
  summarize(first_year = min(year)) |> 
  summarize(first_year_overall = max(first_year)) |> 
  pull()
  )

first_year_overall <- 1991

df_cities_snow_summary <- 
  df_cities_snow |> 
  filter(year >= first_year_overall) |> 
  nest(data = -c(city)) |> 
  mutate(
    last_year_snow = map_int(
      data, 
      function(x) filter(x, snow_2cm) |> 
        summarize(max(year)) |> 
        pull())) |> 
  unnest(data) |> 
  group_by(city) |> 
  summarize(
    n_years = n(),
    n_years_snow = sum(snow_2cm),
    share_years_snow = n_years_snow / n_years,
    last_year_snow = max(last_year_snow)
  ) |> 
  mutate(city = fct_reorder(city, -share_years_snow))
df_cities_snow_summary


## Data for Snowflake ----------------------------------------------------------

# Function to create a fractal snowflake branch
create_fractal_branch <- function(base_x, base_y, angle, depth, length, scale = 0.6) {
  if (depth == 0) return(
    data.frame(x = base_x, y = base_y, xend = base_x + length * cos(angle), 
               yend = base_y + length * sin(angle))
    )
  
  x_end <- base_x + length * cos(angle)
  y_end <- base_y + length * sin(angle)
  
  # Main branch
  main_branch <- data.frame(x = base_x, y = base_y, xend = x_end, yend = y_end)
  
  # Sub-branches
  left_branch <- create_fractal_branch(
    x_end, y_end, angle + pi / 6, depth - 1, length * scale)
  right_branch <- create_fractal_branch(
    x_end, y_end, angle - pi / 6, depth - 1, length * scale)
  
  rbind(main_branch, left_branch, right_branch)
}

# Function to create all branches of the snowflake
create_snowflake <- function(n_branches = 6, depth = 4, length = 1) {
  angles <- seq(0, 2 * pi, length.out = n_branches + 1)[-1]
  do.call(rbind, map(angles, function(x) create_fractal_branch(0, 0, x, depth, length)))
}

# Normalize snowflake to fit within (0, 1) for both x and y dimensions
normalize_snowflake <- function(data) {
  x_range <- range(c(data$x, data$xend))
  y_range <- range(c(data$y, data$yend))
  data$x <- (data$x - x_range[1]) / (x_range[2] - x_range[1])
  data$xend <- (data$xend - x_range[1]) / (x_range[2] - x_range[1])
  data$y <- (data$y - y_range[1]) / (y_range[2] - y_range[1])
  data$yend <- (data$yend - y_range[1]) / (y_range[2] - y_range[1])
  data
}

# Generate the snowflake data
snowflake_data <- create_snowflake(n_branches = 8, depth = 4, length = 1)
snowflake_data <- normalize_snowflake(snowflake_data)


## Circular segment calculations -----------------------------------------------

#' In order to correctly fill the circle so that the area of the circle matches
#' the share
calculate_segment_height_from_share <- function(share) {
  
  if (any(share < 0 | share > 1)) {
    stop("Share values must be between 0 and 1.")
  }
  
  radius <- 0.5 
  
  # Compute the area of a circular segment given the height
  segment_area_function <- function(h) {
    theta <- 2 * acos(1 - h / radius)  # central angle in radians
    area_sector <- (theta / (2 * pi)) * (pi * radius^2)  # area of the sector
    area_triangle <- (radius^2) * sin(theta) / 2  # area of the triangle
    area_sector - area_triangle  # area of the segment
  }
  
  calculate_height <- function(segment_area) {
    if (segment_area == 0) return(0)
    if (segment_area == (pi / 4)) return(radius)
    if (segment_area == (pi / 2)) return(2 * radius)
    
    find_height <- function(h) segment_area_function(h) - segment_area
    uniroot(find_height, c(0, 2 * radius), tol = 1e-6)$root
  }
  
  # Compute segment areas and corresponding heights
  segment_areas <- share * (pi / 4)
  heights <- map_dbl(segment_areas, calculate_height)
  
  heights
}


## Plot ------------------------------------------------------------------------

bg_color <- "#040b29"
main_color <- "skyblue"

plot_titles <- list(
  title = "Weiße Weihnachten?",
  subtitle = sprintf("Anteil der Jahre seit %d mit einer Schneehöhe von 
    mindestens 2 cm<br>
    an mindestens einem Tag zwischen dem 24. und 26.12.", first_year_overall),
  caption = "**Hinweis:** Mindestens 2 cm Schneehöhe an einem der drei Tage.
    **Daten:** DWD Offene Daten. **Visualisierung:** Ansgar Wolsing"
)

df_cities_snow_summary |> 
  ggplot() +
  geom_segment(
    data = snowflake_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = main_color, linewidth = 0.4, lineend = "round") +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, 
        ymin = calculate_segment_height_from_share(share_years_snow), 
        ymax = Inf),
    fill = alpha(bg_color, 0.8)
  ) +
  geom_richtext(
    aes(x = 0.5, y = 0.7, 
        label = sprintf(
          "<b style='font-size: 20px'>%s</b><br>(zuletzt %d)",
          scales::percent(share_years_snow, accuracy = 1),
          last_year_snow
        )
      ),
    color = main_color, fill = NA, family = "Cabinet Grotesk",
    label.size = 0, size = 2.5
  ) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  facet_wrap(vars(city), strip.position = "bottom") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = "transparent", fill = bg_color),
    strip.text = element_text(face = "bold", size = 12),
    strip.clip = "off",
    text = element_text(color = main_color),
    plot.title = element_text(
      family = "Cabinet Grotesk SemiBold", size = 24, hjust = 0.5),
    plot.subtitle = element_markdown(
      hjust = 0.5, lineheight = 1.1, margin = margin(t = 6, b = 12)),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, lineheight = 1.2, margin = margin(t = 15)),
    plot.margin = margin(t = 0, l = 4, r = 4, b = 0),
    panel.spacing.x = unit(6, "mm"),
    panel.spacing.y = unit(7, "mm")
  )
ggsave(here(base_path, "plots", "snow-cities-historical.png"), width = 5, height = 5)


# Bar chart
df_cities_snow_summary |> 
  ggplot() +
  geom_col(
    aes(city, 1),
    fill = NA, color = main_color, width = 0.8
  ) +
  geom_col(
    aes(city, share_years_snow),
    fill = main_color, width = 0.8
  ) +
  geom_richtext(
    aes(city, 1, 
        label = sprintf(
          "<b style='font-size: 20px'>%s</b><br>(zuletzt %d)",
          scales::percent(share_years_snow, accuracy = 1),
          last_year_snow
        )
    ),
    color = main_color, fill = NA, family = "Cabinet Grotesk", 
    label.size = 0, size = 2.5, vjust = 1, nudge_y = -0.02
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_minimal(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = "transparent", fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.clip = "off",
    text = element_text(color = main_color),
    axis.title = element_blank(),
    axis.text = element_text(color = main_color),
    axis.text.x = element_text(
      size = 10, family = "Cabinet Grotesk", face = "bold"),
    plot.title = element_text(
      family = "Cabinet Grotesk SemiBold", size = 24, hjust = 0.5),
    plot.subtitle = element_markdown(
      hjust = 0.5, lineheight = 1.1, margin = margin(t = 6, b = 2)),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, lineheight = 1.2, margin = margin(t = 15)),
    plot.margin = margin(rep(4, 4)),
    panel.spacing.x = unit(6, "mm"),
    panel.spacing.y = unit(7, "mm")
  )
ggsave(here(base_path, "plots", "snow-cities-historical-barchart.png"), width = 5, height = 5)
