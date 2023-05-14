library(tidyverse)
library(ggtext)
library(here)
library(ggflags)
library(ggbump)
library(grid)

base_path <- here("esc2023")

results <- read_tsv(here(base_path, "esc2023-results.tsv"))
results <- results %>% 
  separate(song_artist, into = c("title", "artist"), sep = " \\n")

esc2023_pal <- c("#02025E", "#FFF800", "#FF0087", "#0043FF")
bg_color <- esc2023_pal[1]

# Custom theme
theme_set(
  theme_minimal(base_family = "Outfit", base_size = 9) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
    # panel.grid.minor.y = element_line(color = "white", linewidth = 0.05),
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    # axis.line.y.left = element_line(color = "white"),
    axis.ticks.y.left = element_line(linewidth = 0.2, color = "white"),
    axis.ticks.y.right = element_line(linewidth = 0.2, color = "white"),
    axis.text.x.top = element_text(color = "white", family = "Outfit Medium", size = 10),
    text = element_text(color = "white"),
    plot.title = element_markdown(family = "Outfit Medium", size = 14, hjust = 0.5,
                                  lineheight = 1),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, hjust = 0.5, halign = 0.5, lineheight = 1.25),
    plot.caption = element_markdown(lineheight = 1.1, hjust = 0)
  )
)


results_long <- results %>% 
  pivot_longer(cols = c("points_jury", "points_public"),
               names_to = "voting", values_to = "points", 
               names_transform = function(x) str_remove(x, "points_") %>% str_to_title()) %>% 
  mutate(voting = factor(voting, levels = c("Jury", "Public")),
         country_code = countrycode::countrycode(country, "country.name", "iso2c"),
         country_code = tolower(country_code)) %>% 
  group_by(voting) %>% 
  mutate(rank = rank(points)) %>% 
  ungroup()

p <- results_long %>% 
  ggplot(aes(voting, points, group = country)) +
  geom_bump(
    aes(linewidth = points_total), 
    color = esc2023_pal[4]) + 
  ggflags::geom_flag(aes(country = country_code, size = points)) +
  scale_x_discrete(position = "top", expand = c(add = 0.05, mult = 0)) +
  scale_y_continuous(
    breaks = seq(0, 500, 100), minor_breaks = seq(0, 500, 50),
    sec.axis = dup_axis()) +
  scale_size_area(max_size = 10) +
  scale_linewidth_continuous(range = c(0.2, 1)) +
  coord_cartesian() +
  guides(size = "none", linewidth = "none") +
  labs(
    title = "ESC 2023: Finland were the public's favourites,<br>Sweden won the juries' votes",
    subtitle = "Jury votes vs. public votes",
    caption = "**Source:** Eurovision, European Broadcasting Union.
    **Visualisation:** Ansgar Wolsing",
    x = NULL
  )

ragg::agg_png(here(base_path, "esc2023-results-bumpchart.png"), width = 6, height = 7,
              units = "in", res = 300)
p
grid.segments(x0 = 0.90, y0 = 0, x1 = 1, y1 = 0.10, 
              gp = gpar(col = esc2023_pal[4], lwd = 15, lineend = "square"))
grid.segments(x0 = 0.93, y0 = 0, x1 = 1, y1 = 0.07, 
              gp = gpar(col = esc2023_pal[2], lwd = 15, lineend = "square"))
grid.segments(x0 = 0.96, y0 = 0, x1 = 1, y1 = 0.04,
              gp = gpar(col = esc2023_pal[3], lwd = 15, lineend = "square"))
dev.off()


# With ranks

voting_differences <- results_long %>% 
  group_by(country) %>% 
  mutate(
    points_difference = points - lag(points, 1),
    rank_difference = rank - lag(rank, 1)) %>% 
  ungroup() %>%
  select(country, country_code, points_difference, rank_difference) %>% 
  na.omit()

p <- results_long %>% 
  inner_join(voting_differences, by = c("country", "country_code")) %>% 
  ggplot(aes(voting, rank, group = country)) +
  geom_bump(
    data = ~mutate(., country = fct_reorder(country, points_difference)),
    aes(linewidth = points_total,
        color = abs(points_difference) > 100)) + 
  ggflags::geom_flag(aes(country = country_code, size = points)) +
  scale_x_discrete(position = "top", expand = c(add = 0.1, mult = 0)) +
  scale_y_continuous(
    breaks = seq(0, 500, 100), minor_breaks = seq(0, 500, 50),
    sec.axis = dup_axis()) +
  scale_size_continuous(range = c(3, 8)) +
  scale_linewidth_continuous(range = c(0.2, 1.5)) +
  scale_color_manual(values = c("TRUE" = esc2023_pal[2], "FALSE" = esc2023_pal[4])) +
  coord_cartesian() +
  guides(linewidth = "none", color = "none") +
  labs(
    title = "ESC 2023: Finland were the public's favourites,<br>Sweden won the juries' votes",
    subtitle = sprintf("Country ranks in jury votes vs. public votes.
    Differences of more than 100 points are <span style='color:%s'>highlighted</span>.
    ", esc2023_pal[2]),
    caption = "**Source:** Eurovision, European Broadcasting Union.
    **Visualisation:** Ansgar Wolsing",
    x = NULL, size = "# of points"
  ) + theme(
    axis.text.y.left = element_blank(), 
    axis.text.y.right = element_blank(),
    legend.position = "bottom",
    plot.title = element_markdown(size = 20),
    plot.subtitle = element_textbox(size = 11, margin = margin(b = 12))
  )

ragg::agg_png(here(base_path, "esc2023-results-bumpchart-ranks.png"), width = 6.5, height = 8.5,
              units = "in", res = 300)
p
grid.segments(x0 = 0.90, y0 = 0, x1 = 1, y1 = 0.10, 
              gp = gpar(col = esc2023_pal[4], lwd = 15, lineend = "square"))
grid.segments(x0 = 0.93, y0 = 0, x1 = 1, y1 = 0.07, 
              gp = gpar(col = esc2023_pal[2], lwd = 15, lineend = "square"))
grid.segments(x0 = 0.96, y0 = 0, x1 = 1, y1 = 0.04,
              gp = gpar(col = esc2023_pal[3], lwd = 15, lineend = "square"))
dev.off()
