library(tidyverse)
library(here)
library(rtweet)

first_line <- "I hear the drums echoing tonight"

timeline <- read_rds(here(base_path, "data", filename))

timeline %>% 
  arrange(status_id) %>% 
  select(status_id, text) 


# identify the the first tweet with the first line among retrieved tweets
min_song_start_status_id <- timeline %>% 
  filter(text == first_line) %>% 
  summarize(min(status_id)) %>% 
  pull()

timeline <- timeline %>% 
  filter(status_id >= min_song_start_status_id) %>% 
  arrange(status_id) %>% 
  select(status_id, created_at, text, retweet_count, favorite_count)

line_summary <- timeline %>% 
  group_by(text) %>% 
  summarize(n_retweets = sum(retweet_count),
            n_favorites = sum(retweet_count),
            n_lines = n()) %>% 
  mutate(across(c(n_retweets, n_favorites), function(x) x / n_lines,
                .names = "mean_{.col}"))
  
line_summary %>% 
  mutate(text = fct_reorder(text, n_favorites)) %>% 
  ggplot(aes(text, n_favorites)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL
  )


line_summary %>% 
  mutate(text = fct_reorder(text, mean_n_favorites)) %>% 
  ggplot(aes(text, mean_n_favorites)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL
  )


toto_colors <- c(
  "black",
  "#B5393C"
)


line_summary %>% 
  mutate(text = fct_reorder(text, mean_n_favorites)) %>% 
  ggplot(aes(text, mean_n_favorites)) +
  geom_col(width = 0.4, fill = "white") +
  geom_text(aes(label = text, y = 0), 
            nudge_x = 0.3, family = "Fredericka the Great",
            hjust = 0, col = "grey80") +
  coord_flip() +
  labs(
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = toto_colors[1]),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )
ggsave(here(base_path, "plots", "africabot_favorite_lines.png"), width = 6, height = 7.5)


line_summary %>% 
  mutate(
    text = str_replace_all(text, "[\\[\\]]", "#"),
    text = fct_reorder(text, mean_n_favorites),
         color = ifelse(mean_n_favorites == max(mean_n_favorites), "white", "grey78"),
         label = glue::glue("<span style='color:{color}'>{text} ({round(mean_n_favorites, 2)})</span>"),
         label = fct_reorder(label, mean_n_favorites)) %>% 
  ggplot(aes(label, -mean_n_favorites)) +
  geom_col(aes(fill = color), width = 0.4) +
  # geom_text(aes(label = round(mean_n_favorites, 1)), col = "grey97") +
  scale_x_discrete(position = "top") +
  scale_fill_identity() +
  coord_flip() +
  labs( ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = toto_colors[1]),
    axis.text.y.right = element_markdown("Noto Serif", face = "bold", color = "grey90", size = 7),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
ggsave(here(base_path, "plots", "africabot_favorite_lines.png"), width = 6, height = 5)

