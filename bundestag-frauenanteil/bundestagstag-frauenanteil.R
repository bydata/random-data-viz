library(tidyverse)
library(ggtext)
library(here)

#' Source: Wikipedia
#' Import data to Google Sheet and extract table with IMPORTHTML() 
#' =IMPORTHTML("https://de.wikipedia.org/wiki/Frauenanteil_im_Deutschen_Bundestag_seit_1949";"table";2)

base_path <- "bundestag-frauenanteil"

df <- read_tsv(here(base_path, "wp-bundestag-frauenanteil-partei.tsv"), na = "–")
df <- df %>% 
  mutate(across(-Wahlperiode, 
                function(x) str_remove(x, " %") %>% 
                  str_replace(",", ".") %>% 
                  as.numeric / 100),
         Wahlperiode = str_remove(Wahlperiode, "\\[\\d+\\]"),
         first_year = str_extract(Wahlperiode, "(19|20)\\d{2}") %>% as.numeric(),
         legislaturperiode = str_extract(Wahlperiode, "^\\d{1,2}")
         )

party_colors <- c(
  "CDU/CSU" = "grey9",
  "SPD" = "#ca0002",
  "Grüne" = rgb(100, 161, 45, maxColorValue = 255),
  "FDP" = colorspace::darken("#ffed00", 0.05),
  "Linke" = "purple",
  "AfD" = rgb(0, 158, 224, maxColorValue = 255))



df_long <- df %>% 
  pivot_longer(cols = -c(Wahlperiode, first_year, legislaturperiode),
               names_to = "partei", values_to = "frauenanteil") %>% 
  na.omit() 

df_long %>% 
  ggplot(aes(first_year, frauenanteil, color = partei)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 1, fill = "white") +
  ggrepel::geom_text_repel(
    data = ~subset(., first_year == max(first_year)),
    aes(label = partei),
    hjust = 0, size = 2, direction = "y", nudge_x = 0
  ) +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format()) +
  scale_color_manual(values = party_colors) +
  coord_cartesian(clip = "off", xlim = c(NA, 2024)) +
  # facet_wrap(vars(partei)) +
  guides(color = "none") +
  labs(
    title = "Frauenanteil der Fraktionen im Deutschen Bundestag",
    x = NULL,
    y = "Frauenanteil",
    caption = "Die Linke / Linkspartei hatte in der 15. Legislaturperiode nur 2 Direktmandate, 
    aber keinen Fraktionsstatus. Daten: Wikipedia. Visualisierung: Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey99", fill = "grey99"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot"
  )
ggsave(here(base_path, "bundestag-frauenanteil-alle-parteien-zeitverlauf.png"),
       width = 5, height = 4)


df_long %>% 
  filter(first_year == 2021) %>%
  mutate(partei = fct_reorder(partei, -frauenanteil)) %>% 
  ggplot(aes(partei, frauenanteil)) +
  geom_col(aes(fill = partei == "FDP"), width = 0.8) +
  geom_text(aes(label = scales::percent(frauenanteil, decimal.mark = ",")),
            vjust = 1.5, family = "Roboto Condensed"
            ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = unname(party_colors["FDP"]))) +
  guides(fill = "none") +
  labs(
    title = "Frauenanteil in der FDP-Bundestagsfraktion gering",
    subtitle = "Frauenanteil (%), 20. Legislaturperiode, 2021-",
    caption = "Daten: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey99", fill = "grey99"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    panel.grid = element_blank()
  )
ggsave(here(base_path, "bundestag-frauenanteil-20periode.png"),
       width = 5, height = 4)

