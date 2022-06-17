library(ggplot2)
library(dplyr)
library(here)
library(ggtext)
library(ggimage)

base_path <- here("whoscored-mufc-consistency")

# https://pbs.twimg.com/ad_img/1494624233372192769/lw1ZDT-J?format=jpg&name=900x900

df <- tribble(
  ~opponent, ~home_away, ~rating_ht, ~rating_ft, ~result_ht, ~result_ft,
  "BUR", "h", 6.74, 6.06, "3-1", "3-1",
  "WOL", "h", 6.51, 6.05, "0-0", "0-1",
  "AVL", "h", 6.63, 6.31, "1-0", "1-0",
  "AVL", "a", 6.65, 6.10, "0-1", "2-2",
  "BRE", "a", 6.55, 6.81, "0-0", "1-3",
  "WHU", "h", 6.31, 6.60, "0-0", "1-0",
  "MID", "h", 6.83, 6.29, "1-0", "1-1",
  "BUR", "a", 6.51, 6.08, "0-1", "1-1",
  "SOU", "h", 6.58, 6.10, "1-0", "1-1",
  "BHA", "h", 6.40, 6.81, "0-0", "2-0",
 # "LEE", "a", 6.81, 7.11, "0-2", "2-4"
)

df <- df %>% 
  mutate(image = here(base_path, "input", glue::glue("{opponent}.png")))


ragg::agg_png(here(base_path, "plots", "mufc.png"), res = 300,
              width = 6, height = 6, units = "in")
df %>% 
  mutate(match_id = row_number(),
         opponent_home_away = paste0(opponent, " (", home_away, ")"),
         opponent_home_away = forcats::fct_inorder(opponent_home_away),
         opponent_home_away = forcats::fct_rev(opponent_home_away)) %>% 
  ggplot() +
  geom_segment(aes(x = opponent_home_away, xend = opponent_home_away, 
                   y = rating_ht, yend = rating_ft),
               col = "grey50", size = 1.25) +
  geom_point(aes(opponent_home_away, rating_ht, fill = "Halftime"),
             shape = 21, col = "white", size = 3) +
  geom_point(aes(opponent_home_away, rating_ft, fill = "Fulltime"),
             shape = 21, col = "grey70",  size = 3) +
  geom_text(aes(opponent_home_away, y = pmin(rating_ht, rating_ft), label = opponent_home_away),
            hjust = 0, nudge_x = 0.3, nudge_y = 0.05,
            family = "Roboto Condensed Bold", col = "grey80") +
  geom_image(aes(opponent_home_away, y = pmin(rating_ht, rating_ft), image = image),
             size = 0.03, nudge_x = 0.3, nudge_y = 0.025) +
  scale_fill_manual(values = c("white", "#DF3021")) + 
  coord_flip(ylim = c(6, 7)) +
  guides(fill = "none" # guide_legend(title = NULL)
         ) +
  labs(
    title = toupper("Man Utd. Ratings in the last 10 Matches"),
    subtitle = "DIFFERENCE BETWEEN <b style='color:#DF3021'>FIRST</b> AND <b style='color:white'>SECOND</b> HALF",
    caption = "**Source:** WhoScored.com | **Visualization:** Ansgar Wolsing",
    y = "WhoScored Rating"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "#373B4A"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(size = 0.1, linetype = "dotted", color = "grey89"),
    legend.position = "top",
    legend.justification = "left",
    text = element_text(color = "grey82"),
    plot.title = element_text(color = "#9DD949", face = "bold", size = 18),
    plot.subtitle = element_markdown(size = 12, margin = margin(b = 12)),
    plot.caption = element_markdown(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 0),
    axis.text = element_text(color = "grey84")
  )
dev.off()
