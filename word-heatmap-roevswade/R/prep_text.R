library(tidyverse)
library(tidytext)
library(here)

base_path <- "word-heatmap"

original_tweets <- tweets_combined %>% 
  filter(!is_retweet & lang == "de")

# Stopwords
stopwords_iso_de <- stopwords::stopwords("de", "stopwords-iso")
custom_stopwords <- c("amp", "via")
stopwords <- union(stopwords_iso_de, custom_stopwords)

words <- original_tweets %>% 
  select(status_id, text, user_id) %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  # mutate(word = str_remove(word, "#")) %>% 
  filter(!str_detect(word, "^@"), 
         !str_detect(word, "^#"), 
         !word %in% stopwords,
         str_length(word) > 2)  # minimal word length

# Calculate word correlations
word_threshold <- 10
user_threshold <- 5
words_corr <- words %>% 
  group_by(word) %>% 
  filter(n() >= word_threshold, n_distinct(user_id) >= user_threshold) %>% 
  widyr::pairwise_cor(word, status_id, diag = FALSE)


word_count <- words %>% 
  count(word)

show_n_words <- 30
top_words <- word_count %>% 
  # exclude Michael Ballweg
  filter(!word %in% c("michael", "ballweg")) %>% 
  slice_max(n, n = show_n_words, with_ties = FALSE)

words_corr %>% 
  filter(item1 %in% top_words$word & item2 %in% top_words$word) %>% 
  ggplot(aes(item1, item2)) +
  geom_tile(aes(fill = correlation), col = "white") +
  scale_x_discrete(position = "top") +
  scale_y_discrete() +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange-Gold") +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    axis.text.x.top = element_text(angle = 90, hjust = 0) 
  )
ggsave(here(base_path, "heatmap.png"),
       width = 8, height = 8)
