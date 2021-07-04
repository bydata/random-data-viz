pacman::p_load("tidyverse", "RSQLite", "DBI", "vroom", "dbplyr", "colorspace", "ggfx", "ggtext", "ragg")

# Datasets: https://datasets.imdbws.com/
# Documentation: https://www.imdb.com/interfaces/
# https://db.rstudio.com/databases/sqlite/


# Connect to SQLite database
con <- dbConnect(SQLite(), "data/database/imdb.db")
dbListTables(con)

title_basics <- tbl(con, "title_basics")
title_episode <- tbl(con, "title_episode")
title_ratings <- tbl(con, "title_ratings")

glimpse(title_basics)
glimpse(title_episode)

# Get basic title information for 24
basics_24 <- title_basics %>% 
  filter(primaryTitle == "24", titleType == "tvSeries")

# Parent title ID for 24
parent_title_id <- head(basics_24, 1)  %>% 
  pull(tconst)
parent_title_id

# Get title IDs for episodes
episodes_24 <- title_episode %>% 
  filter(parentTconst == parent_title_id) %>% 
  # inner_join(basics_24, by = c("parentTconst" = "tconst")) %>% 
  inner_join(title_ratings, by = "tconst") %>% 
  arrange(seasonNumber, episodeNumber) %>% 
  collect() %>% 
  # mutate(across(c(seasonNumber, episodeNumber),
  #        ~factor(as.numeric(.x))))
  mutate(across(c(seasonNumber, episodeNumber), as.numeric))
  
glimpse(episodes_24)


# Disconnect from DB
dbDisconnect(con)


# Font: https://www.1001fonts.com/digital+clock-fonts.html
library(extrafont)
font_import(pattern = "digital-7", prompt = FALSE)
loadfonts()

# Color scheme for 24
colors <- c(
  "yellow" = "#FFE669",
  "orange" = "#D18B0F",
  "darkgrey" = "#080A0D",
  "greybrown" = "#6F6F5E",
  "lightgrey" = "#BAC2C2"
)

# Custom ggplot theme
theme_24 <- function(base_family = "Lato", base_size = 8, ...) {
  theme_minimal(base_family = base_family, base_size = base_size, ...) +
  theme(
    plot.background = element_rect(color = NA, fill = colors["darkgrey"]),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, color = lighten(colors["darkgrey"], 0.2)),
    text = element_text(color = "grey89"),
    plot.title = element_markdown(color = "grey98", family = "Lato Black",
                                  size = base_size * 1.75,
                                  margin = margin(t = 12, b = 12)),
    plot.subtitle = element_markdown(size = base_size),
    plot.caption = element_markdown(size = base_size * 0.8),
    axis.text = element_text(color = "grey80"),
    strip.text = element_markdown(color = colors["yellow"], 
                              family = "Digital-7 Mono", size = 18,
                              margin = margin(t = 12, b = 4)),
    legend.position = "top",
    legend.justification = "left"
  )
}

theme_set(theme_24())



# Inspiration: https://twitter.com/CedScherer/status/1242229041488433152

annotate_richtext <- function(label, ...) {
  annotate("richtext", label = label,
           family = "Lato Light", size = 1.75,
           fill = NA, label.color = NA, color = "grey89", label.padding = unit(0.05, "mm"),
           hjust = 0,
           ...)
}

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03, color = "grey80",
             arrow = arrow(length = unit(0.75, "mm"), type = "closed"),
             ...) 
}


average_rating_season <- episodes_24  %>% 
  group_by(seasonNumber) %>% 
  mutate(rating_votes = averageRating * numVotes) %>% 
  summarize(wgt_avg_season_rating = sum(rating_votes) / sum(numVotes),
            avg_season_rating = mean(averageRating))

episodes_24_cont <- episodes_24 %>% 
  arrange(seasonNumber, episodeNumber) %>% 
  mutate(ep_cont = row_number()) %>% 
  inner_join(average_rating_season, by = "seasonNumber")

episodes_24_cont_min <- episodes_24_cont %>% 
  group_by(seasonNumber) %>% 
  summarize(ep_cont_min = min(ep_cont))

titles <- c(
  # "title" = glue::glue("ALL EPISODES OF
  #                         <span style='font-size:28pt; font-family: Digital-7 Mono; color: {colors[1]}'>24</span>
  #                         RATED"),
  # "title" = "ALL EPISODES OF \"24\" RATED",
  "title" = glue::glue("ALL EPISODES OF
                          <span style='color: {colors[1]}'>24</span>
                          RATED"),
  "subtitle" = glue::glue("Each <span style='color: {colors[1]}'>\u2022</span> represents one episode. 
                             <span style='color: {colors[2]}'>Horizontal bars</span> indicate average season ratings (weighted by the number of votes)"),
  "caption" = "Visualization: @_ansgar | Data: IMDB.com")

# https://cran.r-project.org/web/packages/ragg/readme/README.html
ragg::agg_png("plots/24_episode_rating_allseasons_points_nofacets_ragg.png", width = 2400, height = 1200, res = 300)
episodes_24_cont %>% 
  ggplot(aes(ep_cont, averageRating, group = factor(seasonNumber))) +
  geom_segment(aes(xend = ep_cont, 
                   y = wgt_avg_season_rating, yend = averageRating),
               col = colors["greybrown"],
               lty = "solid",
               size = 0.3) +
  with_outer_glow(geom_line(aes(y = wgt_avg_season_rating, 
                col = colors["orange"]),
            size = 0.75, lty = "solid",
  )) +
  geom_point(aes(col = colors["yellow"]),
             size = 0.4
  ) +
  geom_richtext(
    data = episodes_24_cont_min,
    aes(
      x = ep_cont_min,
      y = 7.6,
      label = glue::glue(
        "<span style='font-size:6pt; color: #444444'>Season</span>
         <span style='font-size:12pt; color: #000000'>{seasonNumber}</span>"
      )
    ),
    stat = "unique",
    hjust = 0,
    family = "Digital-7 Mono",
    fill = colors["lightgrey"]
  ) +
  # Annotations
  annotate_richtext(label = "S1 E24 is the best rated<br>episode (9.3)",
           x = 7, y = 9.4) +
  geom_curve2(aes(x = 18.25, xend = 23.25, y = 9.35, yend = 9.3)) +
  annotate_richtext(label = "7.8: S8 E9 has<br>the lowest rating",
                    x = 185, y = 7.85) +
  geom_curve2(aes(x = 185, xend = 181, y = 7.8, yend = 7.8), curvature = -0.2) +
  # Write 24 inside plot
  with_outer_glow(
    annotate("text", family = "Digital-7 Mono", size = 24, label = "24", 
             col = colors["yellow"], hjust = 1, vjust = 1,
           x = 195, y = 10),
    col = colors["orange"],
    sigma = 5, 
    expand = 3) +
  # End annotations
  scale_y_continuous(breaks = seq(8, 9.5, 0.5)) +
  scale_size_continuous(range = c(0.2, 1)) +
  scale_color_identity(
    name = NULL,
    breaks = colors[c("orange", "yellow")],
    labels = c("Weighted season average", "Episode rating"),
    # guide = "legend"
  ) +
  coord_cartesian(ylim = c(7.5, 9.5), clip = "off") +
  labs(title = titles["title"],
       subtitle = titles["subtitle"],
       caption = titles["caption"],
       y = "Average Rating") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
invisible(dev.off())
