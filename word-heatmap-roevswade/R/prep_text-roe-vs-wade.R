library(tidyverse)
library(tidytext)
library(here)
library(textstem)
library(ggtext)
library(glue)

base_path <- "word-heatmap"

original_tweets <- tweets_combined %>% 
  filter(!is_retweet & lang == "en")

# Stopwords
stopwords_iso_en <- stopwords::stopwords("en", "stopwords-iso")
custom_stopwords <- c("amp", "via")
stopwords <- union(stopwords_iso_en, custom_stopwords)

words <- original_tweets %>% 
  select(status_id, text, user_id) %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  # mutate(word = str_remove(word, "#")) %>% 
  filter(!str_detect(word, "^@"), 
         !str_detect(word, "^#"), 
         !word %in% stopwords,
         str_length(word) > 2)  # minimal word length

lemmata <- words %>% 
  mutate(lemma = lemmatize_words(word),
         lemma = ifelse(lemma == "womens", "woman", lemma))

# Calculate word correlations
word_threshold <- 100
user_threshold <- 10
words_corr <- lemmata %>% 
  group_by(lemma) %>% 
  filter(n() >= word_threshold, n_distinct(user_id) >= user_threshold) %>% 
  arrange(lemma) %>% 
  widyr::pairwise_cor(word, status_id, upper = FALSE)

word_count <- words %>% 
  count(word)
lemma_count <- lemmata %>% 
  count(lemma)

show_n_words <- 30
top_words <- word_count %>% 
  slice_max(n, n = show_n_words, with_ties = FALSE)
top_lemmata <- lemma_count %>% 
  slice_max(n, n = show_n_words, with_ties = FALSE)

words_corr_filtered <- words_corr %>% 
  filter(item1 %in% top_lemmata$lemma & item2 %in% top_lemmata$lemma) %>% 
  mutate(
    item1_fmt = ifelse(
      # item1 %in% c("body", "choice"), 
      # item1 == "body",
      item1 == "court",
      glue("<b style='color:#2236AA'>{toupper(item1)}</b>"), item1),
    item2_fmt = ifelse(
      # item2 %in% c("choice", "pro"),
      # item2 == "choice",
      item2 == "supreme",
      glue("<b style='color:#2236AA'>{toupper(item2)}</b>"), item2)
  )

highlight_segments <- tribble(
  ~x1, ~x2,   ~y1, ~y2,    ~pair,
#  5,   0.5,   7,   28.5,  "body-choice",
 11,   0.5,   24,   28.5,  "supreme-court"
)

n_tweets <- nrow(original_tweets)
subtitle_description <- glue(
  "The {show_n_words} most frequent words in **{scales::number(n_tweets, big.mark = ',')} 
  English tweets** with the hashtag #RoeVsWade after the Supreme Court decision are shown.<br>
  The closer the color of a cell is to blue, the **stronger the correlation between two words** - 
  which means if one words appears  in a tweet, the other word is more likely to appear as well. And if one word is absent, the other is more
  likely to be absent, too. For instance, *supreme* (in the rows) appears frequently together with *court* (in columns).
  <br>
  Stopwords (e.g. I, you, the) have been excluded. Excluding retweets.
  ")

p <- words_corr_filtered %>% 
  ggplot(aes(item1, item2)) +
  geom_tile(aes(fill = correlation), col = "white", size = 1) +
  geom_segment(
    data = highlight_segments,
    aes(x = x2, xend = x1, y = y1, yend = y1),
    col = "grey9", size = 0.9
  ) + 
  geom_segment(
    data = highlight_segments,
    aes(x = x1, xend = x1, y = y2, yend = y1),
    col = "grey9", size = 0.9
  ) +  
  geom_point(
    data = highlight_segments,
    aes(x = x1, y = y1),
    col = "grey9", shape = 21, fill = "white", size = 2, stroke = 0.9
  ) + 
  # annotate(GeomTextBox,
  #          x = 24, y = 15,
  #          label = "#RoeVsWade",
  #          family = "Source Serif Pro SemiBold",
  #          box.size = 0, fill = NA,
  #          size = 10, hjust = 0.5, halign = 0.5, vjust = 1,
  #          width = unit(3, "inch"),
  # ) +
  annotate(GeomTextBox,
           x = 22, y = 0,
           label = subtitle_description,
           family = "Source Serif Pro",
           box.size = 0, fill = NA,
           size = 3.25, hjust = 0.5, halign = 0, lineheight = 1.2, vjust = 0,
           width = unit(3.25, "inch"),
  ) +
  scale_x_discrete(position = "top",
                   labels = unique(words_corr_filtered$item1_fmt)) +
  scale_y_discrete(labels = unique(words_corr_filtered$item2_fmt)) + 
  # scale_y_discrete(limits = rev, position = "right") +
  # paletteer::scale_fill_paletteer_c("scico::davos", direction = -1) +
  paletteer::scale_fill_paletteer_c("scico::imola", direction = -1) + 
  coord_fixed(clip = "off") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "#RoeVsWade on Twitter",
    caption = "Source: Twitter Search API (June 24 and June 25, 2022 UTC).
    Visualization: Ansgar Wolsing",
    x = NULL, y = NULL,
    fill = "Correlation"
  ) + 
  theme_minimal(base_family = "Source Serif Pro") +
  theme(
    plot.background = element_rect(color = NA, fill = "#FDFDFD"),
    axis.text = element_markdown(size = 8, family = "Roboto"),
    axis.text.x.top = element_markdown(angle = 90, hjust = 0, vjust = 0.5),
    axis.text.y = element_markdown(),
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "Source Serif Pro SemiBold", hjust = 0.5, size = 32,
      margin = margin(t = 0, b = 18)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(
      hjust = 0.5, margin = margin(t = 12)),
    # legend.position = "bottom",
    # legend.key.width = unit(12, "mm"),
    # legend.key.height = unit(2.5, "mm"),
    legend.position = c(0.85, 0.5),
    legend.direction = "horizontal",
    legend.key.width = unit(7.5, "mm"),
    legend.key.height = unit(2.5, "mm"),
    plot.margin = margin(t = 24, l = 4, r = 4, b = 4)
  )
ggsave(here(base_path, "heatmap.png"), dpi = 400, width = 7.5, height = 7)


## a as network graph ----------------------------------------------------------

library(tidygraph)
library(ggraph)

words_corr_graph <- as_tbl_graph(words_corr_filtered, directed = FALSE) %>% 
  activate(nodes) %>% 
  inner_join(word_count, by = c("name" = "word"))
words_corr_graph

layout <- 

ggraph(words_corr_graph, 
       layout = "linear", sort.by = n, use.numeric = TRUE) +
  geom_edge_arc(aes(
    filter = abs(correlation) > 0.05,
    col = correlation, edge_width = correlation),
                strength = 0.5) +
  geom_node_point(col = "grey98") +
  geom_node_label(aes(label = toupper(name)), hjust = 1, repel = TRUE,
                  max.overlaps = 25, family = "Roboto", segment.color = "white") +
  scale_edge_width(range = c(0.01, 4)) +
  scale_edge_color_viridis(option = "A") +
  # coord_flip(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(l = 40, 12, 12, 12),
    text = element_text(color = "grey98")
  )
ggsave(here(base_path, "network.png"), dpi = 400, width = 10, height = 7)


