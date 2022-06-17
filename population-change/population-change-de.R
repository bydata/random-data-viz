library(tidyverse)
library(ggtext)
library(here)

#' Source: Eurostat
#' Download data from 
#' https://ec.europa.eu/eurostat/databrowser/view/demo_r_gind3/default/table?lang=en
#' https://ec.europa.eu/eurostat/databrowser/bookmark/e4a062d1-617a-45e7-a0fe-f09cd11c3900?lang=en
#' Adaption of Maarten Lambrechts's chart: 
#' https://twitter.com/maartenzam/status/1537705354372558848


base_path <- "population-change"

df <- read_tsv(here(base_path, "demo_r_gind3_page_tabular.tsv"))
head(df)

df <- df %>% 
  separate(1, into = c("freq", "X1", "geo"), sep = ",") %>% 
  select(-c("freq", "X1"))

codes <- tribble(
  ~code, ~abbr, ~name,
  "DE1",	"BW", "Baden-Württemberg",
  "DE2",	"BY", "Bayern",
  "DE3",	"BE", "Berlin",
  "DE4",	"BR", "Brandenburg",
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

df_change <- df %>% 
  inner_join(codes, by = c("geo" = "code")) %>% 
  mutate(pop_change_abs_2020_2021 = `2021` - `2020`,
         pop_change_rel_2020_2021 = pop_change_abs_2020_2021 / `2020`,
         pop_change_abs_2017_2021 = `2021` - `2017`,
         pop_change_rel_2017_2021 = pop_change_abs_2017_2021 / `2017`) %>% 
  arrange(pop_change_rel_2017_2021) 


ragg::agg_png(here(base_path, "population-change-de-2017-2021.png"), units = "in",
              width = 8, height = 6, res = 400)
df_change %>% 
  filter(geo != "DE") %>% 
  arrange(-pop_change_rel_2017_2021) %>% 
  # calculate the cumulative sum of inhabitants
  mutate(cumsum_pop_2017 = cumsum(`2017`),
         cumsum_pop_2017_lag = lag(cumsum_pop_2017, default = 0)) %>% 
  ggplot() +
  geom_rect(aes(xmin = cumsum_pop_2017_lag, xmax = cumsum_pop_2017, 
                ymin = 0, ymax = pop_change_rel_2017_2021,
                fill = pop_change_rel_2017_2021 >= 0),
            col = "white", size = 0.2) +
  geom_label(
    aes(x = cumsum_pop_2017_lag + `2017` / 2, 
        y = pop_change_rel_2017_2021 + ifelse(pop_change_rel_2017_2021 > 0, 0.001, -0.001),
        label = abbr, col = pop_change_rel_2017_2021 >= 0),
    size = 2.5, family = "Roboto Condensed", fill = alpha("white", 0.6), 
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm")) +
  annotate(GeomTextBox,
           x = 50e6, y = -0.01, label = "3 of 4 states losing inhabitants are ",
           family = "Roboto Condensed", box.size = 0, fill = alpha("white", 0.5)) +
  guides(fill = "none",
         color = "none") +
  labs(
    title = "Population Change in German Federal States 2017-2021",
    subtitle = "Total population (widths) multiplied by population change (heights) gives
    growth in absolute numbers (surface area of the rectangles). 
    The width of each rectangle is proportional to the population of
    the federal states in 2017.",
    caption = "Source: Eurostat. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Population Change 2017-2021 (%)"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez", direction = -1), 
                    aesthetics = c("fill", "color")) +
  theme_minimal(base_family = "Roboto Condensed Light") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.2),
    panel.grid.minor.y = element_line(color = "grey80", size = 0.1),
    plot.title = element_text(family = "Roboto Condensed", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(hjust = 0, width = 0.9),
    plot.caption = element_markdown(),
    axis.text.x = element_blank()
  )
invisible(dev.off())

