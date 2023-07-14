library(tidyverse)
library(ggtext)
library(here)
library(gganimate)

base_path <- "climate-global-temperature-monthly-records"

## Load data  ==================================================================

#' Data source: U.S. National Centers for Environmental Information
#' Download data from https://www.ncei.noaa.gov/access/monitoring/global-temperature-anomalies/anomalies
#' Anomalies are with respect to the 20th century average (1901-2000). 
url <- "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/globe/land_ocean/all/12/1850-2023.csv"

anomalies <- read_csv(url, skip = 4, name_repair = tolower)

anomalies <- anomalies %>% 
  mutate(
    month = str_sub(year, 5, 6),
    month = as.numeric(month),
    year = str_sub(year, 1, 4),
    year = as.numeric(year)
  ) %>% 
  select(year, month, anomaly) %>% 
  mutate(
    month_abb = month.abb[month],
    month_abb = factor(month_abb, levels = month.abb)) 

min_year <- min(anomalies$year)
max_year <- max(anomalies$year)


# Custom themes ================================================================

theme_custom_light <- function() {
  theme_minimal(base_family = "Roboto") +
    theme(
      plot.background = element_rect(color = "grey95", fill = "grey95"),
      panel.grid.major.x = element_line(color = "grey70", linewidth = 0.2),
      panel.grid.minor.x = element_line(color = "grey70", linewidth = 0.05),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.ontop = TRUE,
      legend.position = "bottom",
      legend.key.height = unit(2, "mm"),
      text = element_text(lineheight = 1),
      plot.title = element_markdown(face = "bold"),
      plot.title.position = "plot",
      plot.caption = element_markdown(hjust = 0, size = 6)
    )
}

theme_custom_dark <- function() {
  theme_minimal(base_family = "Roboto", base_size = 10) +
    theme(
      plot.background = element_rect(color = "grey2", fill = "grey2"),
      panel.grid.major.x = element_line(color = "grey50", linewidth = 0.2),
      panel.grid.minor.x = element_line(color = "grey50", linewidth = 0.05),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.ontop = TRUE,
      legend.position = "bottom",
      legend.key.height = unit(2, "mm"),
      text = element_text(lineheight = 1, color = "grey80"),
      axis.text = element_text(lineheight = 1, color = "grey80"),
      plot.title = element_markdown(face = "bold", color = "white"),
      plot.title.position = "plot",
      plot.caption = element_markdown(hjust = 0, size = 6),
      plot.subtitle = element_text(margin = margin(b = 12))
    )
}


## PLOTS =======================================================================

# Number of extremes (hot/cold) to present in the chart
n_extremes <- 10

anomalies %>% 
  group_by(month) %>% 
  slice_max(order_by = anomaly, n = n_extremes, with_ties = TRUE) %>% 
  ungroup() %>%
  mutate(anomaly_dir = "max") %>% 
  bind_rows(
    anomalies %>% 
      group_by(month) %>% 
      slice_min(order_by = anomaly, n = n_extremes, with_ties = TRUE) %>% 
      ungroup() %>% 
      mutate(anomaly_dir = "min")
  ) %>% 
  ggplot(aes(year, month)) +
  geom_rect(
    data = ~subset(., month %% 2 == 0),
    aes(xmin = -Inf, xmax = Inf, ymin = month - 0.5, ymax = month + 0.5),
    fill = "grey12"
  ) +
  geom_point(
    # shape = 21, size = 2, col = "black", stroke = 0.2
    aes(col = anomaly),
    # aes(col = anomaly_dir),
    shape = "|", size = 5
  ) +
  scale_x_continuous(breaks = seq(min_year, max_year, 20),
                     minor_breaks = seq(min_year, max_year, 10),
                     expand = c(0, 2), position = "top") +
  scale_y_reverse(breaks = 1:12, labels = month.abb,
                     expand = expansion(add = c(0, 0.5))) +
  paletteer::scale_colour_paletteer_c("grDevices::Blue-Red 2") +
  coord_cartesian(xlim = c(min_year, max_year)) +
  labs(
    title = sprintf("The <span style='color:#D85377'>%d warmest years</span> and
    <span style='color:#8E9CE1'>%d coldest years</span> by month globally",
                    n_extremes, n_extremes),
    subtitle = "Global surface temperature anomalies 1850-2023, land and ocean",
    caption = "Anomalies are with respect to the 20th century average (1901-2000).<br>
    Source: U.S. National Centers for Environmental Information.
    Visualization: Ansgar Wolsing",
    x = NULL, y = NULL,
    col = "Global temperature anomaly (°C)"
  ) +
  theme_custom_dark()
ggsave(here(base_path, "global-temp-anomalies-min-max-top10.png"),
       width = 6, height = 5)



## How many of the last 10 years have been among the 10 hottest on records? ====

anomalies %>% 
  group_by(month) %>% 
  slice_max(order_by = anomaly, n = 10, with_ties = TRUE) %>% 
  ungroup() %>% 
  filter(year >= 2023 - 10) %>% 
  count(month_abb) %>% 
  ggplot(aes(month_abb, n)) +
  geom_col()

anomalies %>% 
  group_by(month) %>% 
  slice_max(order_by = anomaly, n = 10, with_ties = TRUE) %>% 
  # identify biggest anomaly by month
  mutate(anomaly_rank = rank(-anomaly)) %>% 
  ungroup() %>% 
  filter(year >= 2023 - 10) %>% 
  ggplot(aes(month_abb, year)) +
  geom_rect(
    data = ~subset(., year %% 2 == 0),
    aes(xmin = -Inf, xmax = Inf, ymin = year - 0.5, ymax = year + 0.5),
    fill = "grey12"
  ) +
  geom_point(
    aes(size = anomaly),
    col = "white", shape = 22, fill = "grey20"
      ) +
  scale_x_discrete(position = "top", labels = toupper) +
  scale_y_continuous(breaks = 2013:2023) +
  coord_fixed() +
  labs(
    x = NULL, y = NULL
  ) +
  theme_custom_dark() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x.top = element_text(face = "bold")
  )

anomalies %>% 
  group_by(month) %>% 
  slice_max(order_by = anomaly, n = 10, with_ties = TRUE) %>% 
  ungroup() %>% 
  filter(year >= 2023 - 9) %>% 
  ggplot(aes(year, month)) +
  geom_rect(
    data = ~subset(., year %% 2 == 0),
    aes(ymin = -Inf, ymax = Inf, xmin = year - 0.5, xmax = year + 0.5),
    fill = "grey12"
  ) +
  geom_label(
    aes(label = toupper(month_abb)),
    col = "white", size = 2, fill = "grey20", label.size = 0.05,
    label.r = unit(0, "mm")
  ) +
  scale_x_continuous(position = "top", breaks = 2013:2023) +
  scale_y_reverse() +
  labs(
    title = "All the months since 2014 which were among<br>
    the 10 largest positive temperature anomalies on record
    for this month",
    caption = "Global surface temperature anomalies 1850-2023, land and ocean.<br>
    Source: U.S. National Centers for Environmental Information.
    Visualization: Ansgar Wolsing.",
    x = NULL, y = NULL
  ) +
  theme_custom_dark() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x.top = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    plot.title = element_markdown(hjust = 0.5)
  )
ggsave(here(base_path, "global-temp-anomalies-last-10-years-records.png"),
       width = 6, height = 5)


anomalies %>% 
  mutate(
    month_abb = month.abb[month],
    month_abb = factor(month_abb, levels = rev(month.abb))) %>% 
  ggplot(aes(year, anomaly)) +
  geom_col(aes(fill = anomaly)) +
  scale_y_continuous(position = "right") +
  colorspace::scale_fill_continuous_diverging() +
  facet_wrap(vars(month_abb), ncol = 1, switch = "y") +
  theme_custom_dark()

