library(tidyverse)
library(ggtext)
library(grid)
library(here)

base_path <- here("june-heat-de")

# # api_url <- "https://archive-api.open-meteo.com/v1/archive?latitude=50.9333&longitude=6.95&start_date=1940-06-01&end_date=2026-06-24&daily=temperature_2m_mean,temperature_2m_max,temperature_2m_min&timezone=Europe%2FBerlin"
# # Sys.setenv("VROOM_CONNECTION_SIZE" =  2500000)
# # df <- read_csv(api_url)
# df <- read_csv(here(base_path, "open-meteo-50.93N6.91E60m.csv"), skip = 2)

# df_june <- df |> 
#   mutate(
#     year = year(time),
#     day_of_month = mday(time),
#     is_current_year = year == year(today()),
#     is_after_2000 = year > 2000
#   ) |> 
#   filter(month(time) == 6) |> 
#   select(time, year, day_of_month, is_current_year, is_after_2000, everything()) |> 
#   pivot_longer(cols = -c(time:is_after_2000), names_to = "metric")


# df_june |> 
#   filter(metric == "temperature_2m_max (°C)") |> 
#   ggplot(aes(day_of_month, value, group = year, col = is_current_year)) +
#   geom_line(
#     aes(linewidth = ifelse(is_current_year, 1.2, 0.5))
#   ) +
#   scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
#   scale_linewidth_identity()

library(rdwd)
rdwd::updateRdwd() 

# Find stations in location
(station_ids <- rdwd::findID("Koeln", exactmatch=FALSE))

station_ids |> 
  map_dfr(rdwd::metaInfo) |> 
  filter(res == "daily", var == "kl") |> 
  summarize(min(von_datum), max(bis_datum), .by = c(Stations_id, Stationsname)) |> 
  as_tibble()

link <- selectDWD(id = 2667, res = "daily", var = "kl", per = "hr", force = FALSE)
file <- dataDWD(link, read = FALSE)
clim <- readDWD(file, varnames=TRUE)
df <- bind_rows(clim)


df_june <- df |> 
  as_tibble() |> 
  mutate(
    year = year(MESS_DATUM),
    day_of_month = mday(MESS_DATUM),
    is_current_year = year == year(today()),
    is_after_2000 = year > 2000
  ) |> 
  filter(month(MESS_DATUM) == 6) |> 
  select(MESS_DATUM, year, day_of_month, is_current_year, is_after_2000, 
    temp_max = TXK.Lufttemperatur_Max,
    temp_min = TNK.Lufttemperatur_Min) |> 
  distinct() |> 
  add_row(
    MESS_DATUM = as_date(c("2026-06-26", "2026-06-27")),
    year = 2026,
    day_of_month = c(26, 27),
    is_current_year = TRUE,
    is_after_2000 = TRUE,
    temp_max = c(37, 38),
    temp_min = NA_real_
  )

colnames(df_june)



df_june |> 
  filter(temp_max >= 30) |> 
  count(year, sort = TRUE)

df_june |> 
  filter(temp_max >= 30) |> 
  count(year, sort = TRUE) |>
  ggplot(aes(year, n)) +
  geom_col()

max(df_june$year)
df_june |> 
  filter(year == 2026) |> 
  arrange(desc(MESS_DATUM)) |> 
  filter(temp_max >= 30)

df_june |> 
  ggplot(aes(day_of_month, temp_max, group = year, col = is_current_year)) +
  geom_line(
    aes(
      linewidth = ifelse(is_current_year, 1.2, 0.5),
      alpha = ifelse(is_current_year, 1, 0.5)
    )
  ) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  theme_minimal()


df_june |> 
  ggplot(aes(day_of_month, temp_min, group = year, col = is_current_year)) +
  geom_point(size = 0.2, col = "grey30", alpha = 0.3) +
  geom_line(
    data = ~filter(., year == max(year)),
    aes(linewidth = ifelse(is_current_year, 1.2, 0.5))
  ) +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
  scale_linewidth_identity()


df_june |> 
  ggplot(aes(day_of_month, year, fill = temp_max)) +
  geom_tile(
    col = "white", height = 0.8, width = 0.9) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    legend.position = "right"
  )



main_color <- "#dd2a54"

df_june |> 
  ggplot(aes(day_of_month, year, fill = temp_max >= 30)) +
  geom_line(
    aes(day_of_month + 0.5, group = year),
    linewidth = 0.2, col = "grey30"
  ) +
  geom_tile(
    data = ~filter(., temp_max >= 30),
    # aes(alpha = ifelse(year == max(year), 1, 0.7)),
    fill = "white", linewidth = 0.1, height = 0.8, width = 1 # width = 0.9
  ) +
  geom_tile(
    data = ~filter(., temp_max >= 30),
    aes(alpha = ifelse(year == max(year), 1, 0.7)),
    # col = "grey30", 
    linewidth = 0.1, height = 0.8, width = 1 # width = 0.9
  ) +
  # Add forecasted values for June 28th,  2026
  geom_tile(
    data = data.frame(year = 2026, day_of_month = 28, temp_max = 34),
    fill = alpha(main_color, 0.2), col = main_color, 
    linewidth = 0.2, linetype = "dashed", height = 0.8, 
    width = 1,
   # width = 0.9
  ) +
  scale_x_continuous(
    position = "top",
    breaks = seq(1, 30, 7),
    expand = expansion(add = c(0.25, 0.5))
  )  +
  scale_y_continuous(
    breaks = seq(1960, 2030, 5),
    expand = expansion(add = c(0.25, 0))
  ) +
  scale_fill_manual(values = c("FALSE" = "grey96", "TRUE" = "#dd2a54")) +
  scale_alpha_identity() +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  # coord_cartesian(expand = FALSE) +
  guides(fill = "none") +
  labs(
    title = sprintf("Cologne will see <span style='color: %s'>11 consecutive heat days</span>
       in June 2026", main_color),
    subtitle = "Each day in June from 1958 to 2026, measured at Cologne/Bonn Airport weather station",
    caption = "<i>Heat days: air temperature (2m) greater than or equal 30°C<br><br>
      **Source:** Deutscher Wetterdienst (DWD).
      **Visualization:** Ansgar Wolsing",
    x = "Day of the month \U2192",
    y = NULL
  ) +
  theme(
    plot.title = element_markdown(
      family = "Instrument Sans SemiBold", lineheight = 1.25),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.33),
    plot.caption = element_textbox(width = 1),
    axis.title = element_text(size = 9),
    axis.title.x = element_text(hjust = 0),
    axis.text = element_text(family = "Inconsolata", size = 8),
    axis.text.y = element_text(hjust = 0),
    panel.grid = element_line(color = "grey94", linewidth = 0.2)
  )


ragg::agg_png(here(base_path, "june-heat-de-cgn.png"), width = 6, height = 6, units = "in", res = 300)
df_june |> 
  ggplot(aes(day_of_month, year, fill = temp_max >= 30)) +
  geom_tile(
    aes(alpha = ifelse(year == max(year), 1, 0.5)),
    col = "grey30", linewidth = 0.1, height = 0.8, width = 0.9
  ) +
  # Add forecasted values for 2 days in June 2026
  geom_tile(
    data = data.frame(year = 2026, day_of_month = c(27, 28), temp_max = c(39, 34)),
    fill = alpha(main_color, 0.2), col = main_color, 
    linewidth = 0.2, linetype = "solid", height = 0.8, width = 0.9) +
  scale_x_continuous(
    position = "top",
    breaks = seq(1, 30, 7),
    expand = expansion(add = c(0, 0.25))
  )  +
  scale_y_continuous(
    breaks = seq(1960, 2030, 5),
    expand = expansion(add = c(0.25, 0))
  ) +
  scale_fill_manual(values = c("FALSE" = "grey98", "TRUE" = "#dd2a54")) +
  scale_alpha_identity() +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  # coord_cartesian(expand = FALSE) +
  guides(fill = "none") +
  labs(
    title = sprintf("Cologne will see <span style='color: %s'>11 consecutive heat days</span>
       in June 2026", main_color),
    subtitle = "Each day in June from 1958 to 2026, measured at Cologne/Bonn Airport weather station",
    caption = "<i>Heat days: air temperature (2m) greater than or equal 30°C</i><br><br>
      **Source:** Deutscher Wetterdienst (DWD).
      **Visualization:** Ansgar Wolsing",
    x = "Day of the month \U2192",
    y = NULL
  ) +
  theme(
    plot.title = element_markdown(
      family = "Instrument Sans SemiBold", size = 16, lineheight = 1.25),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.33),
    plot.caption = element_textbox(width = 1),
    axis.title = element_text(size = 9),
    axis.title.x = element_text(hjust = 0),
    axis.text = element_text(family = "Inconsolata", size = 8),
    axis.text.x.top = element_text(vjust = 0),
    axis.text.y = element_text(hjust = 0),
    panel.grid = element_blank()
  )
grid.text(
  label = "11 days in 2026 (including\nforecast for June 28th)",
  x = 0.75, y = 0.88, just = "left",
  gp = gpar(fontfamily = "Instrument Sans Italic", fontsize = 7.5, lineheight = 0.95)
)
grid.lines(
  x = 0.9, y = c(0.855, 0.825),
  gp = gpar(lwd = 0.2, fill = "black"),
  arrow = arrow(
    angle = 25, length = unit(1.5, "mm"), type = "closed"
  )
)
dev.off()

# Which years had no heat days in June?
df_june |> 
  summarize(n = sum(temp_max >= 30), .by = year) |> 
  filter(n == 0) |> 
  arrange(year)

# Heat days in June by decade
df_june |> 
  mutate(decade = year %/% 10 * 10) |> 
  summarize(n = sum(temp_max >= 30), .by = c(decade, year)) |> 
  filter(n > 0) |>
  count(decade) |> 
  arrange(decade)

# How many consecutive days per year
df_june_heat_waves_per_year <- df_june |> 
  arrange(MESS_DATUM) |> 
  group_by(year) |> 
  mutate(
    is_heat_day = temp_max >= 30,
    consecutive_heat_days = is_heat_day & lag(is_heat_day),
    consecutive_heat_days = replace_na(consecutive_heat_days, FALSE),
    interrupted = consecutive_heat_days != lead(consecutive_heat_days),
    interrupted = replace_na(interrupted, FALSE),
    session_id = cumsum(interrupted)
  ) |> 
  select(year, MESS_DATUM, temp_max, is_heat_day, consecutive_heat_days, interrupted, session_id) |> #View()
  group_by(year, session_id) |> 
  # by year and session
  summarize(n_heat_days = sum(is_heat_day), .groups = "drop_last") |> 
  # by year
  summarize(
    n_heat_waves = sum(n_heat_days > 0), 
    max_length = max(n_heat_days))

