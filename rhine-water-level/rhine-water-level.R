library(tidyverse)
library(ggtext)

df <- read_csv("~/Downloads/pegel-combined.csv")

df_agg <- df |> 
  mutate(
    year = year(date),
    day = yday(date)) |> 
  filter(year >= 2023) |> 
  filter(water_level > 0) |> 
  summarize(water_level_avg = mean(water_level), .by = c(year, day, date))


# Smoothing for year labels
loess_bw <- 0.1

df_labels <- df_agg |>
  group_by(year) |>
  group_modify(~ {
    last_day <- max(.x$day)
    fit <- loess(water_level_avg ~ day, data = .x, span = loess_bw)
    tibble(
      day = last_day,
      water_level_avg = predict(fit, newdata = tibble(day = last_day))
    )
  }) |>
  ungroup()

df_agg |> 
  ggplot(
    aes(day, water_level_avg, 
      col = year == max(year),
      group = factor(year))) +
  geom_smooth(
    aes(
      linewidth = ifelse(year == max(year), 1.2, 0.5),
      linetype = factor(year)
    ),
    span = loess_bw, se = FALSE) +
  geom_point(
    data = ~filter(., year == max(year)) |> 
      filter(day == max(day)),
    shape = 21, fill = "white", size = 2.5, stroke = 1
  ) +
  geom_text(
    data = df_labels,
    aes(label = year),
    hjust = 0, nudge_x = 4,
    family = "Instrument Sans Medium", size = 3
  ) +
  scale_x_continuous(
    breaks = c(1, 91, 182, 274),
    labels = c("Jan", "Apr", "Jul", "Oct"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = function(x) paste(x, "m")) +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "purple")) +
  scale_linewidth_identity() +
  scale_linetype_manual(
    values = c("2026" = "solid", "2025" = "solid", 
      "2024" = "dashed", "2023" = "dotted", "2022" = "dotdash")) +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(
    color = "none",
    linetype = "none",
    linewidth = "none"
  ) +
  labs(
    title = "Water level of the Rhine in Cologne",
    subtitle = "*Water level (in m, smoothed values)*",
    caption = "***Note:** Hourly water levels smoothed with LOESS (bandwidth = 0.1).*<br><br>
    **Source:** Stadt Köln. **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white", ink = "grey20") +
  theme(
    plot.margin = margin(t = 4, r = 24, b = 4, l = 4),
    plot.title = element_text(family = "Instrument Sans SemiBold", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 0),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("rhine-water-level.png", width = 7.5, height = 5)


df_agg |> 
  ggplot(
    aes(day, water_level_avg, 
      col = year == max(year),
      group = factor(year))) +
  geom_ribbon(
    data = ~filter(., year < max(year)) |> 
      summarize(
        min = min(water_level_avg),
        max = max(water_level_avg),
        .by = day
      ),
    aes(
      x = day, ymin = min, ymax = max
    ),
    fill = "#cccccc88", col = "#cccccc22",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = ~filter(., year == max(year)),
    aes(
      linewidth = ifelse(year == max(year), 1.2, 0.5),
      linetype = factor(year)
    )) +
  geom_point(
    data = ~filter(., year == max(year)) |> 
      filter(day == max(day)),
    shape = 21, fill = "white", size = 2.5, stroke = 1
  ) +
  geom_text(
    data = ~filter(., year == max(year)) |> 
      filter(day == max(day)),
    aes(label = year),
    hjust = 0, nudge_x = 4,
    family = "Instrument Sans SemiBold", size = 3
  ) +
  annotate(
    "text",
    x = 200, y = 5,
    label = "Range 2023-2025",
    family = "Instrument Sans SemiBold", size = 3, color = "#888"
  ) +
  scale_x_continuous(
    breaks = c(1, 91, 182, 274),
    labels = c("Jan", "Apr", "Jul", "Oct"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = function(x) paste(x, "m")) +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "purple")) +
  scale_linewidth_identity() +
  scale_linetype_manual(
    values = c("2026" = "solid", "2025" = "solid", 
      "2024" = "dashed", "2023" = "dotted", "2022" = "dotdash")) +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(
    color = "none",
    linetype = "none",
    linewidth = "none"
  ) +
  labs(
    title = "Water level of the Rhine in Cologne",
    subtitle = "*Water level (in meters)*",
    caption = "**Source:** Stadt Köln. **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white", ink = "grey20") +
  theme(
    plot.margin = margin(t = 4, r = 24, b = 4, l = 4),
    plot.title = element_text(family = "Instrument Sans SemiBold", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 0),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("rhine-water-level-ribbon.png", width = 7.5, height = 5)


df_agg |> 
  ggplot(
    aes(day, water_level_avg, 
      col = year == max(year),
      group = factor(year))) +
  geom_smooth(
    aes(
      linewidth = ifelse(year == max(year), 1.2, 0.25)
    ),
    span = loess_bw, se = FALSE
  ) +
  geom_point(
    data = ~filter(., year == max(year)) |> 
      filter(day == max(day)),
    shape = 21, fill = "white", size = 2.5, stroke = 1
  ) +
  geom_text(
    data = df_labels,
    aes(label = year),
    hjust = 0, nudge_x = 4,
    family = "Instrument Sans Medium", size = 3
  ) +
  scale_x_continuous(
    breaks = c(1, 91, 182, 274),
    labels = c("Jan", "Apr", "Jul", "Oct"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = function(x) paste(x, "m")) +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "purple")) +
  scale_linewidth_identity() +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(
    color = "none",
    linetype = "none",
    linewidth = "none"
  ) +
  labs(
    title = "Water level of the Rhine in Cologne",
    subtitle = "*Water level (in meters)*",
    caption = "***Note:** Hourly water levels smoothed with LOESS (bandwidth = 0.1).*<br><br>
    **Source:** Stadt Köln. **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white", ink = "grey20") +
  theme(
    plot.margin = margin(t = 4, r = 24, b = 4, l = 4),
    plot.title = element_text(family = "Instrument Sans SemiBold", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 0),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("rhine-water-level-lines.png", width = 7.5, height = 5)

