# install.packages("marquee")
# install.packages("palmerpenguins")
library(ggplot2)
library(marquee)
library(palmerpenguins)

# Create the line and point to be added to the subtitle
reg_line_grob <- grid::linesGrob(
  x = c(0, 1), y = 0.5, gp = grid::gpar(lwd = 2, col = "grey23"))
point_grob <- grid::pointsGrob(
  x = 0.5, y = 0.5, pch = 19, gp = grid::gpar(lwd = 2, col = "grey23", cex = 0.5))

penguins |> 
  na.omit() |> 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point(
    aes(fill = species),
    shape = 21, stroke = 0.2, size = 2, color = "white", alpha = 0.9
  ) +
  geom_smooth(
    aes(color = species), method = "lm", se = FALSE) +
  scale_fill_manual(values = c("grey23", "#E1B30F", "#DB880E"),
                    aesthetics = c("fill", "color")) +
  labs(
    title = "Bill depth by bill length for {.grey23 _Adelie_},
    {.#E1B30F _Chinstrap_}, and {.#DB880E _Gentoo_} penguins",
    subtitle = "![](reg_line_grob) = Regression lines by species
    ![](point_grob) = Individual measurements",
    x = "Bill length (mm)", y = "Bill depth (mm)"
  ) +
  theme_minimal(base_family = "Outfit Light") %+replace%
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_marquee(
      family = "Outfit Medium", width = 0.95, hjust = 0, size = 13),
    plot.subtitle = element_marquee(hjust = 0, size = 11),
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ggsave(file.path("marquee-package", "example-underline.png"), 
       width = 5, height = 5)
