library(tidyverse)
library(ggtext)
library(here)

base_path <- "un-human-rights-council-2023"

#' Download data from Our World in Data:
#' https://ourworldindata.org/grapher/distribution-human-rights-index-vdem

df <- read_csv(here(base_path, "distribution-human-rights-index-vdem.csv"))

df <- df %>% 
  filter(Year == 2022, !is.na(Code))

#' https://twitter.com/UNWatch/status/1713929916553548174
council_members <- c(
  "Cuba",
  "Qatar",
  "China",
  "Sudan",
  "Eritrea",
  "Kuwait",
  "Algeria",
  "Burundi",
  "Somalia",
  "Vietnam",
  "Malaysia",
  "Kazakhstan",
  "Bangladesh"
)

# Check if all countries can be found in the dataframe
length(council_members) == 
(df %>% 
  filter(Entity %in% council_members) %>% 
  nrow())

df_plot <- df %>% 
  left_join(data.frame(Entity = council_members, council_member = TRUE)) %>% 
  mutate(council_member = ifelse(is.na(council_member), FALSE, council_member),
    rank = rank(-civ_libs_vdem_owid, ties.method = "first")) 

df_plot %>% 
  ggplot(aes(x = 1, y = civ_libs_vdem_owid, group = 1)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = council_member),
    shape = 21, color = "white", size = 2.5
  ) +
  scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = "#3784CD")) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

df_plot %>% 
  group_by(council_member) %>% 
  mutate(direction_label = ifelse(rank(rank) %% 2 == 0, 1, -1)) %>% 
  ungroup() %>% 
  arrange(rank) %>% 
  ggplot(aes(x = 0, y = rank)) +
  geom_tile(
    aes(fill = council_member), col = "white"
  ) + 
  geom_text(
    data = ~subset(., council_member),
    aes(x = 0.52 * direction_label, label = Entity,
        hjust = ifelse(direction_label == 1, 0, 1)),
    size = 2.5, family = "Source Sans Pro"
  ) +
  # annotate(
  #   "text",
  #   x = 1.1, y = c(1, nrow(df_plot)),
  #   label = c("Most rights \U2192", "\U2190 Fewest rights"),
  #   hjust = c(1, 0),
  #   angle = 90, size = 3, family = "Source Sans Pro SemiBold",
  #   color = "grey50"
  # ) +
  annotate(
    "text",
    x = 0, y = c(-4, nrow(df_plot) + 4),
    label = c("\U2191 Most rights", "\U2193 Fewest rights"),
    hjust = 0.5,
    size = 3, family = "Source Sans Pro SemiBold",
    color = "grey50"
  ) +
  scale_y_reverse() +
  scale_fill_manual(values = c("FALSE" = "grey65", "TRUE" = "#3784CD")) +
  coord_cartesian(xlim = c(-1.2, 1.2)) +
  guides(fill = "none") +
  labs(
    title = "Members of the\nUN Human Rights Council\n2024-2026",
    subtitle = "Ranking of countries by\nHuman Rights Index (V-Dem)",
    caption = "Source: V-Dem, Our World In Data. Visualization: Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 6, hjust = 0.5)
  )
ggsave(here(base_path, "un-hrc-hdi-ranks.png"), width = 3, height = 7)
ggsave(here(base_path, "un-hrc-hdi-ranks.png"), width = 1.5, height = 3.5, scale = 2)
