pacman::p_load("tidyverse", "RSQLite", "DBI", "vroom", "dbplyr", "colorspace", 
               "ggfx", "ggtext", "ragg")

# Datasets: https://datasets.imdbws.com/
# Documentation: https://www.imdb.com/interfaces/
# https://db.rstudio.com/databases/sqlite/

base_path <- here("imdb_ratings_strangerthings")


# Connect to SQLite database
con <- dbConnect(SQLite(), here(base_path, "data/database/imdb.db"))
dbListTables(con)

title_basics <- tbl(con, "title_basics")
title_episode <- tbl(con, "title_episode")
title_ratings <- tbl(con, "title_ratings")

glimpse(title_basics)
glimpse(title_episode)




# Get basic title information for Stranger Things
basics_st <- title_basics %>% 
  filter(primaryTitle == "Stranger Things", titleType == "tvSeries", startYear == "2016")
basics_st

# Parent title ID for 24
parent_title_id <- head(basics_st, 1) %>% pull(tconst)
parent_title_id

# Get title IDs for episodes
episodes_st <- title_episode %>% 
  filter(parentTconst == parent_title_id) %>% 
  # inner_join(basics_st, by = c("parentTconst" = "tconst")) %>% 
  inner_join(title_ratings, by = "tconst") %>% 
  arrange(seasonNumber, episodeNumber) %>% 
  collect() %>% 
  # mutate(across(c(seasonNumber, episodeNumber),
  #        ~factor(as.numeric(.x))))
  mutate(across(c(seasonNumber, episodeNumber), as.numeric))
  
glimpse(episodes_st)

# Disconnect from DB
dbDisconnect(con)


# Color scheme for Stranger Things
colors <- c(
  "yellow" = "#FFE669",
  "orange" = "#D18B0F",
  "darkgrey" = "#080A0D",
  "greybrown" = "#6F6F5E",
  "lightgrey" = "#BAC2C2"
)

# Custom ggplot theme
# ...


# Inspiration: https://twitter.com/CedScherer/status/1242229041488433152

annotate_richtext <- function(label, ...) {
  annotate("richtext", label = label,
           family = "Lato Light", size = 2.75,
           fill = NA, label.color = NA, color = "grey94", label.padding = unit(0.05, "mm"),
           hjust = 0,
           ...)
}

geom_curve2 <- function(..., curvature = 0.1) {
  geom_curve(curvature = curvature, size = 0.1, color = "grey80",
             arrow = arrow(length = unit(0.75, "mm"), type = "closed"),
             ...) 
}

average_rating_season <- episodes_st  %>% 
  group_by(seasonNumber) %>% 
  mutate(rating_votes = averageRating * numVotes) %>% 
  summarize(wgt_avg_season_rating = sum(rating_votes) / sum(numVotes),
            avg_season_rating = mean(averageRating))
average_rating_season

episodes_st_cont <- episodes_st %>% 
  arrange(seasonNumber, episodeNumber) %>% 
  mutate(ep_cont = row_number()) %>% 
  inner_join(average_rating_season, by = "seasonNumber")

episodes_st_cont_summary <- episodes_st_cont %>% 
  group_by(seasonNumber) %>% 
  summarize(ep_cont_min = min(ep_cont),
            ep_cont_median = median(ep_cont))



# Font Kimberley from: https://www.dafont.com/kimberley.font
# main_color <- "#B1271F"
# main_color <- "#84251D"
main_color <- colorspace::lighten("#84251D", 0.2)

titles <- c(
  "title" = "STRANGER THINGS",
  "subtitle" = "
  Stranger Things is one of the most successful series on Netflix. It has an overall rating of 8.7
  on IMDB.
  There is variation between the ratings of the seasons and episodes, which is shown in this plot. 
  Each **dot** represents the average IMDB rating of an episode. The **horizontal bars** indicate 
                          average season ratings (weighted by the number of votes).",
  "caption" = "Data: IMDB.com. Visualization: Ansgar Wolsing")

ragg::agg_png(here(base_path, "plots/strangerthings_episode_ratings.png"), 
              width = 10, height = 6, res = 600, units = "in")
episodes_st_cont %>% 
  # extend ep_cont for extended lines
  group_by(seasonNumber) %>% 
  mutate(ep_cont_extended = case_when(
    ep_cont == min(ep_cont) ~ as.numeric(ep_cont) - 0.25,
    ep_cont == max(ep_cont) ~ as.numeric(ep_cont) + 0.25,
    TRUE ~ as.numeric(ep_cont)
  )) %>% 
  ungroup() %>% 
  ggplot(aes(ep_cont, averageRating, group = factor(seasonNumber))) +
  geom_curve(
    aes(xend = ep_cont, y = wgt_avg_season_rating, yend = averageRating),
    col = main_color,  lty = "solid",  size = 0.5, curvature = 0.2) +
  with_shadow(
    geom_line(
      aes(ep_cont_extended, y = wgt_avg_season_rating),
      col = "#84251D", size = 3, lty = "solid"),
    colour = "grey2", expand = 0.75, lineend = "butt", 
    ) +
  with_outer_glow(
    geom_point(color = "grey80", size = 3),
    expand = 15, colour = main_color, sigma = 21
  ) +
    geom_richtext(
      data = episodes_st_cont_summary,
      aes(
        x = ep_cont_median, y = 10.25,
        label = glue::glue(
          "<span style='font-size:9pt; color: grey72'>Season</span>
         <span style='font-size:24pt; color: #84251D'>{seasonNumber}</span>"
        )
      ),
      stat = "unique", hjust = 0.5, family = "Benguiat", fill = NA, label.size = 0
  ) + 
  # Annotations
  annotate_richtext(label = "S2 E7 is odd<br>in many ways (6.1)",
           x = 12, y = 6) +
  # geom_curve2(aes(x = 8.25, xend = 13.75, y = 6.1, yend = 6.1)) +
  coord_cartesian(ylim = c(6, 10), clip = "off") +
  guides(
    color = "none"
  ) +
  labs(title = titles["title"],
       subtitle = titles["subtitle"],
       caption = titles["caption"],
       y = "Average Rating") +
  theme_minimal(base_family = "Montserrat") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey9"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "grey62"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey82"),
    plot.title = element_markdown(
      family = "Benguiat", color = "#84251D", size = 28, hjust = 0.5,
      margin = margin(t = 6)),
    plot.subtitle = element_textbox(
      margin = margin(t = 10, b = 28), width = 0.8, hjust = 0.5, halign = 0.5,
      lineheight = 1.25
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey20", size = 0.2),
    panel.grid.minor.y = element_line(color = "grey20", size = 0.1)
    )
invisible(dev.off())

#' S2 E7: 
#' https://www.digitalspy.com/tv/ustv/a841946/stranger-things-season-2-episode-7-the-lost-sister-what-went-wrong/

