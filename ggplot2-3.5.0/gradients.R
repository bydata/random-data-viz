library(ggplot2)
library(dplyr)

colors <- c("#0E0E52", "#150578", "#192BC2")
gradient_fill <- grid::linearGradient(colors, group = FALSE)

mtcars |>
  count(cyl) |>
  ggplot(aes(factor(cyl), n)) +
  geom_col(fill = gradient_fill, width = 0.75) +
  geom_text(
    aes(label = n), 
     fontface = "bold", color = "white", vjust = 1.5, size = 5, 
    family = "Outfit", show.legend = FALSE
  ) +
  labs(
    title = "Bars with gradient fill",
    x = "Number of cylinders") +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.text.x = element_text(),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.margin = margin(rep(3, 4))
  )
ggsave(file.path("ggplot2-3.5.0", "gradient-bars.png"), 
       width = 4, height = 4, scale = 1.25)


mtcars |>
  count(cyl) |>
  ggplot(aes(factor(cyl), n)) +
  geom_col(fill = "white", width = 0.75) +
  geom_text(
    aes(label = n), 
    fontface = "bold", color = "white", vjust = -0.5, size = 5, 
    family = "Outfit", show.legend = FALSE
  ) +
  labs(
    title = "Background with gradient fill",
    x = "Number of cylinders") +
  theme_void(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = NA, fill = gradient_fill),
    text = element_text(color = "white"),
    axis.title.x = element_text(margin = margin(t = 4)),
    axis.text.x = element_text(),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.margin = margin(rep(3, 4))
  )
ggsave(file.path("ggplot2-3.5.0", "gradient-bg.png"), width = 4, height = 4, scale = 1.25)
