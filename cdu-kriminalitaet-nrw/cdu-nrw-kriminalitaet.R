library(tidyverse)
library(ggtext)
library(here)

base_path <- "cdu-kriminalitaet-nrw"

df <- read_tsv(here(base_path, "pks-aufklaerungsquote-bundeslaender-2021.tsv"))
glimpse(df)

stadtstaaten <- c("Berlin", "Hamburg", "Bremen")


df %>% 
  mutate(bundesland_label = ifelse(bundesland == "Nordrhein-Westfalen", "**Nordrhein-Westfalen**",
                                    bundesland),
         bundesland_label = fct_reorder(bundesland_label, aufklquote),
         type = ifelse(bundesland %in% stadtstaaten, "Stadtstaat", "Flächenstaat"),
         type2 = ifelse(bundesland == "Nordrhein-Westfalen", "NRW", type)) %>% 
ggplot(aes(bundesland_label, aufklquote)) +
  geom_col(aes(fill = type2), width = 0.8) +
  geom_text(aes(label = scales::percent(aufklquote/100, decimal.mark = ",")),
            color = "white", hjust = 1.1, size = 2.5, fontface = "bold",
            family = "Noto Sans") + 
  geom_richtext(aes(label = bundesland_label, y = 0),
            color = "white", fill = NA, label.colour = NA, hjust = 0, size = 2.5, 
            family = "Noto Sans") + 
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  scale_fill_manual(values = c("grey50", "steelblue", "grey75")) + 
  coord_flip() +
  guides(fill = "none") + 
  labs(
    title = "<span style='color:steelblue'>NRW</span> auch 2021 Schlusslicht unter den
    <span style='color:grey50'>Flächenländern</span>",
    subtitle = "Aufklärungsquote (Kriminalität gesamt) laut PKS der Ländern in %",
    caption = "Quellen: Polizeiliche Kriminalstatistik der Länder. Visualisierung: Ansgar Wolsing",
    x = NULL, y = "Aufklärungsquote"
  ) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    plot.title = element_markdown(face = "bold", color = "grey2"),
    plot.caption = element_markdown(hjust = 0, size = 6),
    text = element_text(color = "grey20"),
    plot.background = element_rect(color = NA, fill = "white")
  )
ggsave(here(base_path, "aufklquote.png"), width = 5.25, height = 5)
