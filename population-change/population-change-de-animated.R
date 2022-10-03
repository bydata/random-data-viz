library(tidyverse)
library(ggtext)
library(lubridate)
library(gganimate)
library(here)

#' Source: Destatis
#' Code: 12411-0010
#' https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0010&bypass=true&levelindex=0&levelid=1656265628146#abreadcrumb

base_path <- "population-change"

pop_flat <- read_csv2(here(base_path, "12411-0010_flat.csv"), locale = locale(encoding = "ISO-8859-15"))
head(pop_flat)

codes <- tribble(
  ~code, ~abbr, ~name,
  "DE1",	"BW", "Baden-Württemberg",
  "DE2",	"BY", "Bayern",
  "DE3",	"BE", "Berlin",
  "DE4",	"BB", "Brandenburg",
  "DE5",	"HB", "Bremen",
  "DE6",	"HH", "Hamburg",
  "DE7",	"HE", "Hessen",
  "DE8",	"MV", "Mecklenburg-Vorpommern",
  "DE9",	"NI", "Niedersachsen",
  "DEA",	"NW", "Nordrhein-Westfalen",
  "DEB",	"RP", "Rheinland-Pfalz",
  "DEC",	"SL", "Saarland",
  "DED",	"SN", "Sachsen",
  "DEE",	"ST", "Sachsen-Anhalt",
  "DEF",	"SH", "Schleswig-Holstein",
  "DEG",	"TH", "Thüringen"
)

pop_prep <- pop_flat %>% 
  mutate(year = as.numeric(str_sub(Zeit, 5, 8))) %>% 
  rename(bundesland = `1_Auspraegung_Label`, pop = BEVSTD__Bevoelkerungsstand__Anzahl) %>% 
  inner_join(codes, by = c("bundesland" = "name")) %>% 
  select(bundesland, abbr, year, pop) %>% 
  arrange(bundesland, year) %>% 
  group_by(bundesland) %>% 
  mutate(pop_prev_year = lag(pop),
         pop_first_year = .$pop[which(.$year == 1990 & .$bundesland == bundesland)],
         pop_change_prev_year = pop / pop_prev_year - 1,
         pop_change_first_year = pop / pop_first_year - 1) %>% 
  ungroup() %>% 
  # since the change columns are missing for the first year in the dataset, 
  # we remove the first year
  filter(year > min(year)) %>% 
  group_by(year) %>% 
  arrange(-pop_prev_year, .by_group = TRUE) %>% 
  mutate(pop_prev_year_cumsum = cumsum(pop_prev_year),
         pop_prev_year_cumsum_lag = lag(pop_prev_year_cumsum, default = 0)) %>% 
  ungroup()


# Custom theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed") +
    theme(
      plot.background = element_rect(color = NA, fill = "white"),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", size = 0.2),
      panel.grid.minor.y = element_line(color = "grey80", size = 0.1),
      text = element_text(color = "grey32"),
      plot.title = element_text(family = "Roboto Condensed", face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        hjust = 0, width = 0.9, margin = margin(t = 2, b = 8)),
      plot.caption = element_markdown(hjust = 0),
      axis.text.x = element_blank()
    )
)


# states to abbreviate
states_use_abbr <- c("BE", "HH", "BB", "SH", "RP", "HB", "MV",
                     "SL", "TH", "NI")

p <- pop_prep %>% 
  mutate(name = str_replace(bundesland, "-", "-<br>"),
         label = ifelse(abbr %in% states_use_abbr, abbr, name)
  ) %>% 
  # filter(year == 1992) %>% 
  ggplot() +
  geom_rect(aes(xmin = pop_prev_year_cumsum_lag, xmax = pop_prev_year_cumsum, 
                ymin = 0, ymax = pop_change_prev_year,
                fill = pop_change_prev_year >= 0),
            col = "white", size = 0.35) +
  geom_richtext(
    aes(x = pop_prev_year_cumsum_lag + pop_prev_year / 2, 
        y = pop_change_prev_year + ifelse(pop_change_prev_year > 0, 0.0005, -0.0005),
        label = label, col = pop_change_prev_year >= 0,
        vjust = ifelse(pop_change_prev_year > 0, 0, 1)),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm"),
    lineheight = 0.95) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Egypt", direction = 1), 
                    aesthetics = c("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Population Change in German Federal States 2017-2021",
    subtitle = "Total population (widths) multiplied by population change (heights) gives
    growth in absolute numbers (surface area of the rectangles). 
    The width of each rectangle is proportional to the population of
    the federal states in 2017.",
    caption = "Source: Destatis. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Population Change 2017-2021 (%)"
  )

p_anim <- p + transition_states(year)
animate(p_anim, res = 150, width = 6, height = 5, units = "in")  



p <- pop_prep %>% 
  mutate(name = str_replace(bundesland, "-", "-<br>"),
         label = ifelse(abbr %in% states_use_abbr, abbr, name),
         year = factor(year)) %>% 
  # filter(year == 1992) %>% 
  ggplot() +
  geom_rect(aes(xmin = pop_prev_year_cumsum_lag, xmax = pop_prev_year_cumsum, 
                ymin = 0, ymax = pop_change_first_year,
                fill = pop_change_first_year >= 0),
            col = "white", size = 0.35) +
  geom_richtext(
    aes(x = pop_prev_year_cumsum_lag + pop_prev_year / 2, 
        y = pop_change_first_year + ifelse(pop_change_first_year > 0, 0.0005, -0.0005),
        label = label, col = pop_change_first_year >= 0,
        vjust = ifelse(pop_change_first_year > 0, 0, 1)),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm"),
    lineheight = 0.95) +
  geom_text(aes(x = 0, y = -0.15, label = year),
            stat = "unique", size = 16, color = "grey50", hjust = 0, vjust = 0) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Egypt", direction = 1), 
                    aesthetics = c("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Annual Population Change in German Federal States 1990 to 2021",
    caption = "**BE** Berlin, **HH** Hamburg, **BB** Brandenburg,
         **SH** Schleswig-Holstein, **RP** Rheinland-Pfalz<br>
         **HB** Bremen, **MV** Mecklenburg-Vorpommern,
         **SL** Saarland, **TH** Thüringen, **NI** Niedersachsen<br><br>
    Source: Destatis. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Population Change since 1990 (%)"
  )


p <- pop_prep %>% 
  group_by(year) %>% 
  arrange(-pop_change_first_year, .by_group = TRUE) %>% 
  mutate(pop_prev_year_cumsum = cumsum(pop_prev_year),
         pop_prev_year_cumsum_lag = lag(pop_prev_year_cumsum, default = 0)) %>% 
  ungroup() %>% 
  mutate(name = str_replace(bundesland, "-", "-<br>"),
         label = ifelse(abbr %in% states_use_abbr, abbr, name),
         year = factor(year)) %>% 
  # filter(year == 1992) %>% 
  ggplot() +
  geom_rect(aes(xmin = pop_prev_year_cumsum_lag, xmax = pop_prev_year_cumsum, 
                ymin = 0, ymax = pop_change_first_year,
                fill = pop_change_first_year >= 0),
            col = "white", size = 0.35) +
  geom_richtext(
    aes(x = pop_prev_year_cumsum_lag + pop_prev_year / 2, 
        y = pop_change_first_year + ifelse(pop_change_first_year > 0, 0.0005, -0.0005),
        label = label, col = pop_change_first_year >= 0,
        vjust = ifelse(pop_change_first_year > 0, 0, 1)),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm"),
    lineheight = 0.95) +
  geom_text(aes(x = 0, y = -0.15, label = year),
            stat = "unique", size = 16, color = "grey68", hjust = 0, vjust = 0) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Egypt", direction = 1), 
                    aesthetics = c("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Population Change in German Federal States since 1990",
    subtitle = "The width of each rectangle is proportional to the population of
    the federal states in the previous year.",
    caption = "**BB** Brandenburg, **BE** Berlin, **HB** Bremen,
         **HH** Hamburg, **NI** Niedersachsen, **MV** Mecklenburg-Vorpommern,<br>
         **RP** Rheinland-Pfalz,  **SH** Schleswig-Holstein, **SL** Saarland, 
         **TH** Thüringen<br><br>
    Source: Destatis. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Population Change since 1990 (%)"
  )

p_anim <- p + transition_states(year)
animate(p_anim, res = 200, width = 6, height = 5, units = "in",
        end_pause = 12, duration = 30, bg = "white")  
anim_save(here(base_path, "population-change-de-since-1990.gif"))




## With federal states coloured between East and West ==========================



p <- pop_prep %>% 
  group_by(year) %>% 
  arrange(-pop_change_first_year, .by_group = TRUE) %>% 
  mutate(pop_prev_year_cumsum = cumsum(pop_prev_year),
         pop_prev_year_cumsum_lag = lag(pop_prev_year_cumsum, default = 0)) %>% 
  ungroup() %>% 
  mutate(name = str_replace(bundesland, "-", "-<br>"),
         label = ifelse(abbr %in% states_use_abbr, abbr, name),
         year = factor(year),
         ost_west = case_when(
           abbr == "BE" ~ "Ost/West",
           abbr %in% c("SN", "ST", "TH", "BB", "MV") ~ "Ost",
           TRUE ~ "West"
         )
  ) %>% 
  ggplot() +
  geom_rect(aes(xmin = pop_prev_year_cumsum_lag, xmax = pop_prev_year_cumsum, 
                ymin = 0, ymax = pop_change_first_year,
                fill = ost_west),
            col = "white", size = 0.35) +
  geom_richtext(
    aes(x = pop_prev_year_cumsum_lag + pop_prev_year / 2, 
        y = pop_change_first_year + ifelse(pop_change_first_year > 0, 0.0005, -0.0005),
        label = label, col = ost_west,
        vjust = ifelse(pop_change_first_year > 0, 0, 1)),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm"),
    lineheight = 0.95) +
  geom_text(aes(x = 0, y = -0.15, label = year),
            stat = "unique", size = 16, color = "grey68", hjust = 0, vjust = 0) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Lakota", direction = 1), 
                    aesthetics = c("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Population Change in German Federal States since 1990",
    subtitle = "The width of each rectangle is proportional to the population of
    the federal states in the previous year.",
    caption = "**BB** Brandenburg, **BE** Berlin, **HB** Bremen,
         **HH** Hamburg, **NI** Niedersachsen, **MV** Mecklenburg-Vorpommern,<br>
         **RP** Rheinland-Pfalz,  **SH** Schleswig-Holstein, **SL** Saarland, 
         **TH** Thüringen<br><br>
    Source: Destatis. Visualization: Ansgar Wolsing (Inspiration: Maarten Lambrechts)",
    x = NULL,
    y = "Population Change since 1990 (%)"
  )

p_anim <- p + transition_states(year)
animate(p_anim, res = 200, width = 6, height = 5, units = "in",
        end_pause = 12, duration = 30, bg = "white")  
anim_save(here(base_path, "population-change-de-since-1990-ostwest.gif"))




p <- pop_prep %>% 
  group_by(year) %>% 
  arrange(-pop_change_first_year, .by_group = TRUE) %>% 
  mutate(pop_prev_year_cumsum = cumsum(pop_prev_year),
         pop_prev_year_cumsum_lag = lag(pop_prev_year_cumsum, default = 0)) %>% 
  ungroup() %>% 
  mutate(name = str_replace(bundesland, "-", "-<br>"),
         label = ifelse(abbr %in% states_use_abbr, abbr, name),
         year = factor(year),
         ost_west = case_when(
           abbr == "BE" ~ "Ost/West",
           abbr %in% c("SN", "ST", "TH", "BB", "MV") ~ "Ost",
           TRUE ~ "West"
         )
  ) %>% 
  ggplot() +
  geom_rect(aes(xmin = pop_prev_year_cumsum_lag, xmax = pop_prev_year_cumsum, 
                ymin = 0, ymax = pop_change_first_year,
                fill = ost_west),
            col = "white", size = 0.35) +
  geom_richtext(
    aes(x = pop_prev_year_cumsum_lag + pop_prev_year / 2, 
        y = pop_change_first_year + ifelse(pop_change_first_year > 0, 0.0005, -0.0005),
        label = label, col = ost_west,
        vjust = ifelse(pop_change_first_year > 0, 0, 1)),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm"),
    lineheight = 0.95) +
  geom_text(aes(x = 0, y = -0.15, label = year),
            stat = "unique", size = 16, color = "grey68", hjust = 0, vjust = 0) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Lakota", direction = 1), 
                    aesthetics = c("fill", "color")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Bevölkerungsentwicklung in den Bundesländern seit 1990",
    subtitle = "Die Breite der Rechtecke ist proportional zur Bevölkerungszahl
    der Bundesländern im Vorjahr.",
    caption = "**BB** Brandenburg, **BE** Berlin, **HB** Bremen,
         **HH** Hamburg, **NI** Niedersachsen, **MV** Mecklenburg-Vorpommern,<br>
         **RP** Rheinland-Pfalz,  **SH** Schleswig-Holstein, **SL** Saarland, 
         **TH** Thüringen<br><br>
    Daten: Statistisches Bundesamt. Visualisierung: Ansgar Wolsing
    (Inspiration: Maarten Lambrechts)",
    x = NULL,
    y = "Bevölkerungsentwicklung seit 1990 (%)"
  )

p_anim <- p + transition_states(year)
animate(p_anim, res = 200, width = 6, height = 5, units = "in",
        end_pause = 12, duration = 30, bg = "white")  
anim_save(here(base_path, "population-change-de-since-1990-ostwest-de.gif"))



