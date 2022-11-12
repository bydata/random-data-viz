library(tidyverse)
library(ggtext)
library(here)

base_path <- "bvb-laufleistung"

df <- tribble(
  ~spiel, ~laufleistung_heim, ~laufleistung_ausw,
"KOEBVB", 121, 116.8,
"SEVBVB", 113.6, 117.5,
"BVBFCB", 117.3, 117.2,
"BVBSEV", 114.3, 117.9,
"FCUBVB", 124.8, 113.1,
"BVBVfB", 108.4, 106.5,
"BVBMCI", 116.4, 115.4,
"SGEBVB", 115.6, 111.4,
"COPBVB", 132.1, 122.6,
"BVBBOC", 105, 107.6,
"WOBBVB", 117, 108.9,
"BMGBVB", 118.7, 109.3
)

df <- df %>% 
  mutate(team_heim = str_sub(spiel, 1, 3),
         team_ausw = str_sub(spiel, 4, 6)) %>% 
  mutate(laufleistung_bvb = ifelse(team_heim == "BVB", laufleistung_heim, laufleistung_ausw),
         laufleistung_gegner = ifelse(team_heim != "BVB", laufleistung_heim, laufleistung_ausw),
         spiel_id = row_number()) %>% 
  select(spiel_id, spiel, laufleistung_bvb, laufleistung_gegner) # %>% 
  # pivot_longer(cols = -spiel, names_to = "team", values_to = "laufleistung",
  #              names_transform = function(x) str_remove(x, "laufleistung_"))
  

df %>% 
  add_row(
    spiel_id = max(.$spiel_id + 1),
    spiel = NA,
    laufleistung_bvb = 109,
    laufleistung_gegner = NA
  ) %>% 
  ggplot(aes(spiel_id, laufleistung_bvb)) +
  geom_segment(
    aes(x = spiel_id + 0.5, xend = spiel_id + 0.5, 
        y = laufleistung_bvb, yend = laufleistung_gegner,
        color = laufleistung_bvb > laufleistung_gegner),
    size = 7
  ) +
  geom_step(size = 1) +
  geom_label(
    aes(x = spiel_id + 0.5, 
        y = laufleistung_gegner + ifelse(
          laufleistung_gegner > laufleistung_bvb, 1, -1),  
        label = spiel),
    fill = alpha("white", 0.7), label.size = 0, size = 2.5,
    family = "Roboto Condensed"
  ) +
  scale_x_continuous(breaks = NULL) +
  # scale_x_continuous(breaks = seq_len(nrow(df)), labels = unique(df$spiel)) +
  scale_y_continuous(limits = c(100, NA)) +
  colorspace::scale_color_discrete_diverging(palette = "Tropic", rev = TRUE) +
  guides(color = "none") +
  labs(
    title = "Laufleistung BVB vs. Gegner",
    subtitle = "Die Linie zeigt die Laufleistung des BVB, die Balken die der gegnerischen Mannschaften",
    x = NULL, y = "Laufleistung (in km)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(face = "bold")
  )
ggsave(here(base_path, "laufleistung-bvb.png"), width = 7, height = 5)


laufleistung_torwart <- 5

df %>% 
  mutate(across(starts_with("laufleistung_"), ~(.x - laufleistung_torwart) / 10, .names = "{.col}_je_spieler"),
         laufleistung_diff = laufleistung_bvb - laufleistung_gegner,
         laufleistung_diff_rel = laufleistung_diff / laufleistung_bvb_je_spieler
         ) %>% 
  ggplot(aes(spiel_id, laufleistung_diff_rel)) +
  geom_col(aes(fill = laufleistung_bvb > laufleistung_gegner), width = 0.7) +
  geom_label(
    aes(x = spiel_id, 
        y = laufleistung_diff_rel + ifelse(
          laufleistung_gegner > laufleistung_bvb, -0.05, 0.05),  
        label = spiel),
    fill = alpha("white", 0.7), label.size = 0, size = 2.5,
    family = "Roboto Condensed"
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = seq(-1, 0.6, 0.2)) +
  colorspace::scale_fill_discrete_diverging(palette = "Tropic", rev = TRUE) +
  guides(fill = "none") +
  labs(
    title = "Wie ist Union Berlin für einen Spieler mehr gelaufen als der BVB?",
    subtitle = "Laufleistung BVB vs. Gegner",
    x = NULL,
    y = "Unterschied Laufleistung in Spielern",
    caption = sprintf("%d km von der Team-Laufleistung für den Torwart abgezogen", laufleistung_torwart)
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(face = "bold")
  )
ggsave(here(base_path, "laufleistung-bvb-relativ.png"), width = 7, height = 5)
