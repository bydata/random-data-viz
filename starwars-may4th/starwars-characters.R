library(tidyverse)
library(ggtext)
library(here)

#' Download Star Wars Database from Kaggle
#' https://www.kaggle.com/jsphyg/star-wars

characters <- read_csv("starwars-may4th/characters.csv")

summary(characters)
table(characters$species)

star_wars_logo_color <- "#F7E51D"

characters %>% 
  filter(!is.na(height) & !is.na(mass)) %>% 
  ggplot(aes(height, mass)) +
  geom_jitter(size = 2.5, shape = 21, color = "white", stroke = 0.1, 
              height = 0.1, width = 0.1,
             fill = star_wars_logo_color) +
  annotate("text",
           x = 150, y = 1500,
           label = "Who's this?", color = star_wars_logo_color,
           family = "Raleway SemiBold"
           ) +
  geom_curve(data = NULL,
             aes(x = 150, xend = 173, y = 1450, yend = 1335),
             color = star_wars_logo_color,
             arrow = arrow(length = unit(2, "mm"))) +
  labs(
    title = "STAR WARS CHARACTERS",
    subtitle = "Height and mass of characters",
    caption = "**Source:** The Star Wars Wiki, Kaggle (jsphyg) | **Visualization:** Ansgar Wolsing",
    x = "Height (cm)",
    y = "Mass (kg)"
  ) +
  theme_minimal(base_family = "Raleway") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey2"),
    text = element_text(color = "grey90"),
    plot.title = element_text(color = star_wars_logo_color,
                              family = "Bebas Neue", size = 24, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 8)),
    axis.text = element_text(color = "grey90"),
    panel.grid.major = element_line(size = 0.3, color = "grey19"),
    panel.grid.minor = element_line(size = 0.2, color = "grey19")
  )
ggsave("starwars-may4th/star-wars-scatter-height-mass.png", dpi = 400, 
       width = 6, height = 6)
