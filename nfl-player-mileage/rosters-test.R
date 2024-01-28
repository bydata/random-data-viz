library(tidyverse)
library(ggtext)
library(nflverse)

base_path <- "nfl-player-mileage"

rosters <- load_rosters(seasons = most_recent_season())
seasons <- 1989:2023
rosters <- load_rosters(seasons = seasons)
write_rds(rosters, here(base_path, "rosters-1989-2023.rds"))
colnames(rosters)


# Positions need clean-up
rosters %>% 
  filter(season == most_recent_season()) %>% 
  count(position)
rosters %>% 
  filter(position == "CB") %>% 
  count(depth_chart_position)
rosters %>% 
  filter(position == "SS") %>% 
  count(depth_chart_position)
rosters %>% 
  filter(position == "S") %>% 
  count(depth_chart_position)
rosters %>% 
  filter(position == "OLB") %>% 
  count(season, depth_chart_position)
rosters %>% 
  filter(season == most_recent_season(), position == "LB") %>% 
  count(season, depth_chart_position)
rosters %>% 
  filter(position == "DT") %>% 
  count(season, depth_chart_position)

rosters <- rosters %>% 
  mutate(position_unified = case_when(
    position %in% c("DB", "CB", "S", "SS", "FS") ~ "DB",
    position %in% c("MLB", "OLB", "LB", "ILB") ~ "LB",
    position %in% c("DL", "DT", "DE") ~ "DL",
    position %in% c("OL", "C", "G") ~ "OL",
    TRUE ~ position
  ))

positions_abbr_mapping <- c(
  "DB" = "Defensive Back",
  "DL" = "Defensive Line",
  "LB" = "Linebacker",
  "OL" = "Offensive Line",
  "QB" = "Quarterback",
  "RB" = "Running Back",
  "WR" = "Wide Receiver",
  "T"  = "Tackle",
  "TE" = "Tight End",
  "K"  = "Kicker"
)

player_career_seasons <- rosters %>% 
  # keep only players who had their rookie season in the first season pulled
  filter(entry_year >= min(season)) %>% 
  mutate(player_id = paste(full_name, gsis_id, sep = "#")) %>% 
  group_by(player_id, full_name) %>% 
  summarize(
    seasons_active_n = n_distinct(season), 
    seasons_span = max(season) - min(season) + 1,
    position = first(position_unified),
    retired = as.logical(max(season) < most_recent_season()),
    .groups = "drop") %>% 
  arrange(-seasons_active_n)

player_career_seasons %>% 
  count(position, sort = TRUE)

player_career_seasons %>% 
  group_by(position) %>% 
  summarize(
    seasons_span_mean = mean(seasons_span),
    seasons_span_sd = sd(seasons_span),
    seasons_span_median = median(seasons_span),
    players_n = n()
  ) %>% 
  filter(players_n >= 100) %>% 
  arrange(-seasons_span_mean)


player_career_seasons %>% 
  add_count(position, name = "players_n") %>% 
  filter(players_n >= 200) %>% 
  ggplot(aes(position, seasons_span)) +
  geom_jitter(
    alpha = 0.8,
    shape = 21
  ) +
  stat_summary(
    color = "red"
  ) +
  coord_flip()


library(ggdist)

player_career_seasons %>% 
  filter(retired) %>% 
  add_count(position, name = "players_n") %>% 
  filter(players_n >= 200) %>% 
  ggplot(aes(position, seasons_span)) +
  stat_halfeye() +
  coord_flip()


library(survival)
library(survminer)

player_career_seasons_filtered <- player_career_seasons %>% 
  group_by(position) %>% 
  filter(n() >= 200) %>% 
  ungroup()

player_career_seasons_filtered %>% 
  count(position)

survival_data <- with(player_career_seasons_filtered, Surv(seasons_span, retired))
fit <- survfit(survival_data ~ position, data = player_career_seasons_filtered)
plot(fit, col = rainbow(length(unique(player_career_seasons_filtered$position))))
summary(fit)

# Log-rank test
result_logrank <- survdiff(Surv(seasons_span, retired) ~ position, data = player_career_seasons_filtered)
result_logrank


fit_tidy <- broom::tidy(fit) %>% 
  mutate(
    position_abbr = str_remove(strata, "position="),
    position = positions_abbr_mapping[position_abbr])

fit_tidy %>% 
  # add 100 % at time=0
  bind_rows(
    data.frame(position = unique(fit_tidy$position), time = 0, estimate = 1)
  ) %>% 
  arrange(position, time) %>% 
  ggplot(aes(time, estimate, group = position)) +
  geom_smooth(color = "grey70", size = 0.5, se = FALSE) +
  geom_smooth(
    data = ~mutate(., position2 = position),
    aes(group = position2), 
    color = "steelblue", span = 0.5, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1),
    expand = expansion(add = c(0, 0.1))) +
  facet_wrap(vars(position2)) +
  theme_light()

fit_tidy %>% 
  filter(estimate <= 0.5) %>% 
  group_by(position) %>% 
  filter(time == min(time)) %>% 
  ungroup() %>% 
  arrange(-time, -estimate)

# How many 
fit_tidy %>% 
  filter(time %in% c(3, 5, 10)) %>% 
  arrange(time, estimate) %>% 
  mutate(position = fct_inorder(position)) %>% 
  select(position, time, n.risk, estimate) %>% 
  ggplot(aes(position, estimate)) +
  geom_col(
    aes(fill = position %in% c("Quarterback", "Running Back")),
    width = 0.75, show.legend = FALSE, #fill = "#123265"
    ) +
  geom_text(
    data = ~filter(., time == min(time)),
    aes(
      x = as.numeric(position), y = 0.02,
      label = position),
    hjust = 0, family = "Source Sans Pro", color = "white", size = 2.5
  ) +
  geom_text(
    aes(
      label = scales::percent(estimate, accuracy = 1),
      hjust = ifelse(estimate > 0.15, 1.2, -0.2),
      color = ifelse(estimate > 0.15, "white", "grey30")),
    size = 2.5, family = "Chivo"
  ) +
  scale_color_identity() +
  scale_fill_manual(values = c("FALSE" = "#123265", "TRUE" = "#C32C20")) +
  coord_flip() +
  labs(
    title = "Different Career expectations",
    subtitle = "While 20 % of Quarterbacks are 
    expected to pursue an active career 
    10 years after entering the NFL, it's only 3 % of the Running Backs",
    caption = "Estimates from a survival analysis (Kaplan-Meier). NFL players who
    had their rookie season between 1989 and 2023. 
    Data: NFL.com via {nflreadr} R package. Visualization: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  facet_wrap(vars(time), labeller = as_labeller(function(x) paste(x, "seasons"))) +
  theme_minimal(base_family = "Source Sans Pro Light", base_size = 10) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    axis.text = element_blank(),
    text = element_text(color = "grey20"),
    plot.title = element_markdown(family = "Bangers", size = 16, color = "grey2"),
    plot.subtitle = element_textbox(
      width = 0.95, lineheight = 1.1, margin = margin(t = 4, b = 4)),
    plot.caption = element_textbox(
      width = 0.95, hjust = 0, size = 7, family = "Source Sans Pro Light",
      lineheight = 1),
    strip.text = element_text(
      family = "Source Sans Pro SemiBold", size = 10, hjust = 0),
    panel.grid = element_blank()
  )
ggsave(here(base_path, "nfl-active-different-year-facets.png"),
       width = 5, height = 4)


player_career_seasons_filtered %>% 
  count(position)

player_career_seasons_filtered %>% #
  filter(position == "DB") %>% 
  count(seasons_span)

player_career_seasons_filtered %>% #
  filter(position == "QB", seasons_span >= 15) %>% 
  arrange(-seasons_span) %>% 
  select(full_name, seasons_span)

#' Quarterbacks with careers spanning at least 10 seasons NOT winning a Super Bowl title
#' Josh McCown 19
#' Matt Hasselbeck 17, NFC Champion
#' Matt Schaub 17, NFC Champion
#' Philip Rivers 17
#' Ryan Fitzpatrick 17
#' Alex Smith 16, NFC Champion
#' Josh Johnson 16,
#' Brian Hoyer 15 * (Champion, but not as a starter for NE)
#' Carson Palmer 15
#' Chad Henne 15 * (Champion, but not as a start for KC)
#' Matt Ryan 15
#' --> 9 without a title
