library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)
library(glue)
library(lubridate)

base_path <- "champions-league-finals"

cl_history_url <- "https://fbref.com/en/comps/8/history/UEFA-Champions-League-Seasons"
season_end_years <- 1993:2021

# match_urls <- get_match_urls(country = "", gender = "M", season_end_year = 2021, 
#                              non_dom_league_url = cl_history_url)

if (FALSE) {
  match_urls <- map(season_end_years, 
                    ~get_match_urls(
                      country = "", gender = "M", season_end_year = .x, 
                      non_dom_league_url = cl_history_url))
  write_lines(match_urls, here(base_path, "champions-league-match-urls.txt"))
} else {
  match_urls <- read_lines(here(base_path, "champions-league-match-urls.txt"))
}


# identify the final from the date in the match urls
get_final_match_url <- function(match_urls, season_end_year) {
  m_d_y <- str_extract(match_urls, sprintf("((?:%s)-\\d{1,2}-\\d{4})", paste(month.name, collapse = "|"))) 
  dates <- mdy(m_d_y)
  match_urls[which.max(dates)]
  
}
cl_final_match_urls <- map2_chr(match_urls, season_end_years, get_final_match_url)

# get match summaries
match_summaries <- map(cl_final_match_urls, get_match_summary)
match_summaries <- set_names(match_summaries, season_end_years)


map_df(match_summaries, head, 1) %>% 
  select(Season, Home_Score, Away_Score) %>% 
  count(Home_Score, Away_Score) 

map_df(match_summaries, head, 1) %>% 
  select(Season, Home_Score, Away_Score) %>% 
  mutate(winner_score = ifelse(Home_Score > Away_Score, Home_Score, Away_Score),
         loser_score = ifelse(Home_Score <= Away_Score, Home_Score, Away_Score)) %>% 
  count(winner_score, loser_score, sort = TRUE) %>% 
  ggplot(aes(winner_score, loser_score)) +
  geom_point(aes(size = n), shape = 15, col = "white") +
  geom_text(aes(label = n), col = "grey20", family = "Chivo") +
  annotate(GeomTextBox, 
          x = 3, y = 2.3,
          label = "Liverpool fans might remember this one.", 
          box.r = unit(0.2, "mm"),
          box.size = 0, fill = alpha("white", 0.1), col = "white",
          family = "Fira Sans", size = 3
          ) +
  annotate(GeomCurve,
           x = 2.55, xend = 2.9, y = 2.55, yend = 2.85, col = "white",
           curvature = 0.1, size = 0.2,
           arrow = arrow(length = unit(1.5, "mm"))) +
  scale_x_continuous(position = "top") +
  scale_y_reverse(expand = expansion(add = 0.42)) +
  scale_size_area(max_size = 16) +
  coord_fixed() +
  guides(size = "none") +
  labs(
    title = "Goals in Champions League Finals",
    subtitle = "Number of goals scored by each team in Champions League finals 
    since 1993. Goals in extra time are included, goals from penalty shootouts are not.",
    caption = "**Source:** FBRef.com. **Visualization:** Ansgar Wolsing",
    x = "Winner's goals \U2192",
    y = "\U2190 Opponent's goals"
  ) +
  theme_minimal(base_family = "Chivo") +
  theme(
    plot.background = element_rect(color = NA, fill = "#1A3358"),
    panel.grid.major = element_line(size = 0.3, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title.x.top = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    plot.subtitle = element_textbox_simple(
      family = "Fira Sans", size = 9, margin = margin(t = 2, b = 6)),
    plot.caption = element_markdown(family = "Fira Sans"),
    plot.title.position = "plot"
  )
ggsave(here(base_path, "results.png"), width = 5, height = 5)


# Goals scored after 90th minute
match_summaries %>% 
  bind_rows() %>% 
  filter(Event_Time > 90 & Event_Time <= 120 & Event_Type == "Goal") %>% View()


library(ggsankey)

df_plot <- match_summaries %>% 
  bind_rows() %>% 
  filter((Event_Type == "Goal" | 
            (Event_Type == "Penalty" & !str_detect(Event_Players, "Penalty Miss")))
                                    & !Is_Pens) %>% 
  tibble() %>% 
  # select(Season, Team, Home_Away, Event_Time, Score_Progression) %>% 
  select(Season, Event_Time, Score_Progression) %>% 
  arrange(Event_Time) %>% 
  separate(Score_Progression, sep = ":", convert = TRUE, remove = FALSE,
           into = c("Home_Score_Progression", "Away_Score_Progression")) %>% 
  group_by(Season) %>% 
  # add_row(Season = first(Season), Event_Time = NA, 
  #          Home_Score_Progression = last(Home_Score_Progression),
  #          Away_Score_Progression = last(Away_Score_Progression)) %>% 
  mutate(Event_Id = row_number(),
         next_Event_Id = lead(Event_Id),
         next_node = lead(Score_Progression, default = "END")) %>% 
  ungroup()

df_plot %>% 
  ggplot(aes(x = Event_Id, next_x = next_Event_Id, group = Season,
            node = Score_Progression,  next_node = next_node,
           label = Score_Progression)) +
  geom_sankey() +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  guides(fill = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8")
  )

match_summaries %>% 
  bind_rows() %>% 
  filter(Event_Type == "Penalty")

