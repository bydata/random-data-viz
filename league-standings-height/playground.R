library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)

base_path <- "league-standings-height"

seasons <- 1992:2021


# Pull the season final tables. Source: Transfermarkt
final_matchday_tables <- map(
  seasons,
  ~tm_matchday_table(
    country_name = "England", start_year = .x, 
    # in the first 3 seasons (1992-92 to 1994-95), 24 teams played in EPL
    matchday = ifelse(.x <= 1994, 42, 38)))
final_matchday_tables <- set_names(final_matchday_tables, seasons)
write_rds(final_matchday_tables, here(base_path, "final_matchday_tables.rds"))

final_matchdays <- bind_rows(final_matchday_tables, .id = "season")
# since the function returns duplicates, remove them from the result
final_matchdays <- final_matchdays %>% 
  distinct()

final_matchdays %>% 
  filter(season >= "1995") %>% 
  mutate(season_fmt = paste(
    str_sub(season, 3, 4), 
    str_sub(as.character(as.numeric(season) + 1), 3, 4),
    sep = "-"
  ),
  season_fmt = fct_inorder(season_fmt),
  squad_highlights = case_when(
    squad %in% c("Man City", "Man Utd", "Liverpool", "Chelsea", "Arsenal") ~ squad,
    TRUE ~ "Other teams"
  )) %>% 
  ggplot(aes(season_fmt, pts, group = factor(rk))) +
  # indicate relegation
  geom_rect(
    data = ~subset(., rk == 18),
    aes(xmin = as.numeric(season_fmt) - 0.25, xmax = as.numeric(season_fmt) + 0.25, 
        ymin = 6, ymax = pts),
    stat = "unique", fill = "grey84"
  ) +
  geom_text(
    data = ~subset(., season == "1995"),
    aes(y = 8, label = "Relegation zone"),
    stat = "unique", angle = 90, size = 2, color = "grey10", hjust = 0, 
    family = "Noto Sans"
  ) +
  ggbeeswarm::geom_beeswarm(
    aes(fill = squad_highlights),
    size = 2, shape = 22, color = "grey90", cex = 0.7) +
  geom_text(
    data = ~subset(., rk == 1),
    aes(label = squad),
    size = 1.5, nudge_y = 2, family = "Noto Sans Kannada Light"
  ) +
  # Annotate season in the plot
  geom_label(aes(y = 106, label = season_fmt),
            size = 2, family = "Chivo", color = "grey40", label.size = 0, 
            label.r = unit(0, "mm"), fill = "white") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(position = "right", expand = expansion(add = c(1, 2))) +
  scale_fill_manual(values = c("Man City" = "skyblue",
                               "Liverpool" = "#C8102E", "Man Utd" = "darkred",
                               "Chelsea" = "darkblue", "Arsenal" = "#9C824A",
                               "Other teams" = "grey51")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Premier League Point Distribution per Season",
    subtitle = "Each season since 1995-1996 - the first season with 20 teams in the 
    Premier League - is represented by a column.<br>The teams are placed based on 
    their points in the final table. The points that led to relegation are shown 
    with a bit darker grey area.",
    caption = "**Data:** Transfermarkt, worldfootballR R package.
    **Visualization:** Ansgar Wolsing",
    x = NULL, #"Season",
    y = "Points",
    fill = NULL
  ) +
  theme_minimal(base_family = "Noto Sans", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(size = 7.5, color = "grey94"),
    panel.grid.major.y = element_line(size = 0.1, color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.text.x.top = element_blank(),
    text = element_text(color = "grey23"),
    plot.title = element_text(color = "grey2", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 4, b = 8)
    ),
    plot.caption = element_markdown(hjust = 1),
    legend.position = "bottom"
  )
ggsave(here(base_path, "premier-league-final-tables.png"), width = 9.5, height = 5)

