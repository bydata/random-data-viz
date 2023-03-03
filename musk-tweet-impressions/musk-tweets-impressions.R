library(tidyverse)
library(ggtext)
library(rvest)
library(lubridate)
library(fontawesome)

#' URL pattern to get the tweets for a given time period without replies and without retweets
#' https://twitter.com/search?q=from%3Aelonmusk%20since%3A2022-12-31%20until%3A2023-01-01%20-filter%3Aretweets%20AND%20-filter%3Areplies&src=typed_query&f=live

path <- "/Users/ansgar/Downloads/tweets-elonmusk/"
# path <- "/Users/ansgar/Downloads/tweets-elonmusk/with-replies"
(files <- list.files(path, ".html"))
filenames <- file.path(path, files)

parse_tweets <- function(file) {
  page <- read_html(file)
  
  # get tweet div containers
  tweet_elems <- page %>% 
    # html_nodes(css = "div:nth-child(3) > div > section > div > div > div")
    html_nodes(xpath = "//div/div[3]/div/section/div/div/div[@data-testid='cellInnerDiv']")
  # remove the last item
  tweet_elems <- tweet_elems[seq_len(length(tweet_elems) - 1)]
  
  # parse the timestamps from the source code
  timestamps <- tweet_elems %>% 
    html_nodes(css = "a time") %>% 
    html_attr("datetime")
  
  # parse span items (every 4th is a view count item)
  impressions <- tweet_elems %>% 
    html_nodes(css = "div:nth-child(4) > a > div > div > span > span > span") %>% 
    html_text()
  
  # determine whether tweet is a reply or not
  is_reply <- tweet_elems %>% 
    html_text() %>% 
    str_detect("Antwort an")
  
  # return a data frame with timestamps and impression counts  
  data.frame(
    timestamp = timestamps,
    impressions = clean_impression_count_items(impressions),
    is_reply = is_reply
    #impressions_raw = impressions
  )
}

clean_impression_count_items <- function(x) {
  million_pattern <- "(m|Mio\\.)"
  val <- x %>% 
    # replace big mark "." with ""
    str_replace("\\.", "") %>% 
    str_remove("\\s(m|Mio\\.?)") %>%
    str_replace(",", ".") %>% 
    str_squish() %>% 
    as.numeric()
  val <- ifelse(str_detect(x, million_pattern), 1e6, 1) * val
  val
}


tweet_impressions <- map_dfr(filenames, parse_tweets)

# keep only the latest version of a tweet
tweet_impressions <- tweet_impressions %>% 
  arrange(timestamp) %>% 
  group_by(timestamp) %>% 
  slice_max(order_by = impressions, n = 1, with_ties = FALSE) %>% 
  ungroup()



tweet_impressions %>% 
  distinct() %>% 
  mutate(date = as_date(timestamp)) %>% 
  group_by(date) %>% 
  summarize(avg_impressions = mean(impressions)) %>% 
  ggplot(aes(date, avg_impressions)) +
  geom_line() +
  geom_vline(aes(xintercept = as_date("2023-02-13")), col = "red")

tweet_impressions %>% 
  distinct() %>% 
  mutate(date = as_date(timestamp),
         week = floor_date(date, "1 week"),
  ) %>% 
  group_by(week) %>% 
  summarize(avg_impressions = mean(impressions)) %>% 
  ggplot(aes(week, avg_impressions)) +
  geom_col() +
  geom_vline(aes(xintercept = as_date("2023-02-13")), col = "red")

tweet_impressions %>% 
  distinct() %>% 
  mutate(date = as_date(timestamp),
         week = floor_date(date, "1 week"),
  ) %>% 
  group_by(week) %>% 
  summarize(median_impressions = median(impressions)) %>% 
  ggplot(aes(week, median_impressions)) +
  geom_col() +
  geom_vline(aes(xintercept = as_date("2023-02-13")), col = "red")

tweet_impressions %>% 
  mutate(date = as_date(timestamp),
         week = floor_date(date, "1 week", week_start = 1),
  ) %>% 
  group_by(week) %>% 
  summarize(total_impressions = sum(impressions)) %>% 
  ggplot(aes(week, total_impressions)) +
  geom_col() +
  geom_vline(aes(xintercept = as_date("2023-02-13")), col = "red")


##########

twitter_png <- fa_png("twitter", 
                      file.path("musk-tweet-impressions", "twitter-icon.png"),
                      height = 60, width = 60)
twitter_label <- sprintf("<img src='%s' width=12 height=12>", 
                         file.path("musk-tweet-impressions", "twitter-icon.png"))

p1 <- tweet_impressions %>% 
  mutate(date = as_date(timestamp),
         week = floor_date(date, "1 week", week_start = 1),
  ) %>% 
  # exclude most recent day
  filter(date < max(date)) %>% 
  ggplot(aes(week, y = impressions, group = week,)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = week >= as_date("2023-02-13")),
    shape = 21, color = "white", varwidth = 0.2, alpha = 0.9
  ) +
  stat_summary(
    aes(group = 1),
    geom = "line", fun = median, linetype = "dashed"
  ) +
  stat_summary(
    geom = "point", fun = median, shape = 21, fill = "grey98", size = 2.5
  ) +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = rev(MetBrewer::met.brewer("Klimt", 2))) +
  guides(fill = "none") +
  labs(
    x = NULL,
    y = "Tweet impressions"
  ) +
  theme_minimal(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.15, color = "grey80"),
    panel.grid.minor.y = element_line(linewidth = 0.08, color = "grey80")
  )

# replace points with Twitter birds using beeswarm point locations 
layer_data_summary_points <- layer_data(p1, 3)
p1$layers[[3]] <- NULL
p1_birds <- p1 + 
  geom_richtext(
    data = layer_data_summary_points,
    aes(x = as_date(x), y, label = twitter_label),
    inherit.aes = FALSE,
    label.size = 0, fill = NA) 
# p1_birds_layers_ordered <- p1_birds
# p1_birds_layers_ordered$layers[[1]] <- p1_birds$layers[[3]]
# p1_birds_layers_ordered$layers[[2]] <- p1_birds$layers[[1]]
# p1_birds_layers_ordered$layers[[3]] <- p1_birds$layers[[2]]

# ragg::agg_png(file.path("musk-tweet-impressions", "em-tweets-impressions-beeswarm-by-week.png"), 
#               width = 10, height = 8, units = "in", res = 100)
# p1_birds_layers_ordered
# invisible(dev.off())
p1_birds
ggsave(file.path("musk-tweet-impressions", "em-tweets-impressions-beeswarm-by-week.png"),
       width = 5, height = 4)


tweet_impressions %>%
  mutate(date = as_date(timestamp),
         after_superbowl = timestamp >= as_date("2023-02-13"),
         after_superbowl_1d = timestamp >= as_date("2023-02-14"),
         after_superbowl_1d = ifelse(after_superbowl_1d, "After 2023-02-13", "Before")
         ) %>% 
  # exclude most recent day
  filter(date < max(date)) %>% 
  ggplot(aes(x = after_superbowl_1d, impressions)) +
  geom_boxplot(
    aes(fill = after_superbowl_1d),
    color = "grey40", linewidth = 0.2, width = 0.2, outlier.color = NA, alpha = 0.2,) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = after_superbowl_1d),
    shape = 21, col = "white", size = 2, varwidth = 0.2
    ) +
  annotate(
    "text",
    x = c(1.2, 2.5), 
    y = 1e6, 
    label = c("After Super Bowl", "Before Super Bowl"),
    family = "Chivo", hjust = 0, color = "grey36", size = 3
  ) +
  # scale_x_reverse() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = MetBrewer::met.brewer("Klimt", 2)) +
  coord_flip() +
  # facet_wrap(vars(is_reply), ncol = 1) +
  guides(fill = "none") +
  labs(
    title = "Impressions per Tweet",
    y = "Impressions", 
    x = NULL
  ) +
  theme_minimal(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.15, color = "grey80"),
    panel.grid.minor.x = element_line(linewidth = 0.08, color = "grey80"),
    # axis.text = element_text(family = "Chivo")
    axis.text.y = element_blank()
  )
ggsave(file.path("musk-tweet-impressions", "em-tweets-impressions-beeswarm.png"), 
       width = 5, height = 4)


#' Source: https://www.platformer.news/p/yes-elon-musk-created-a-special-system
# 
# sum(!tweet_impressions$is_reply)
# 
# tweet_impressions %>% 
#   mutate(date = as_date(timestamp),
#          after_superbowl = timestamp >= as_date("2023-02-13"),
#          after_superbowl_1d = timestamp >= as_date("2023-02-14")) %>% 
#   group_by(is_reply, after_superbowl_1d) %>% 
#   summarize(median(impressions))

tweet_impressions %>%
  mutate(date = as_date(timestamp),
         after_superbowl = timestamp >= as_date("2023-02-13"),
         after_superbowl_1d = timestamp >= as_date("2023-02-14")) %>%
  # exclude most recent day
  filter(date < max(date)) %>% 
  group_by(after_superbowl_1d) %>%
  summarize(median(impressions))

tweet_impressions2 <- tweet_impressions %>%
  mutate(date = as_date(timestamp),
         after_superbowl = timestamp >= as_date("2023-02-13"),
         after_superbowl_1d = timestamp >= as_date("2023-02-14")) %>%
  # exclude most recent day
  filter(date < max(date)) 

tweet_impressions2 %>% 
  ggplot(aes(impressions, fill = after_superbowl_1d)) +
  geom_histogram(color = "white") +
  facet_wrap(vars(after_superbowl_1d), scales = "free_y")

tweet_impressions_after_superbowl_1d <- 
  tweet_impressions2$impressions[tweet_impressions2$after_superbowl_1d]


resamples <- 10
means <- vector("double", resamples)
i <- 0
for (i in seq_len(resamples)) {
  tweet_impressions_before_superbowl_1d_sample <- sample(
    tweet_impressions2$impressions[!tweet_impressions2$after_superbowl_1d],
    size = length(tweet_impressions_after_superbowl_1d),
    replace = FALSE
  )
  means[i] <- mean(tweet_impressions_after_superbowl_1d > tweet_impressions_before_superbowl_1d_sample)
}
mean(means)
sd(means)


# twitter_svg <- fa("twitter")
# twitter_png <- fa_png("twitter", 
#                       file.path("musk-tweet-impressions", "twitter-icon.png"),
#                       height = 60, width = 60)

# twitter_label <- sprintf("<img src='%s' width=12 height=12>", 
#                          file.path("musk-tweet-impressions", "twitter-icon.png"))
# iris %>% 
#   ggplot(aes(Sepal.Length, Sepal.Width)) +
#   geom_richtext(aes(label = twitter_label))


tweet_impressions %>% 
  mutate(timestamp = as_datetime(timestamp)) %>% 
  ggplot(aes(timestamp)) +
  geom_segment(aes(xend = timestamp, y = 0, yend = impressions,
                   col = timestamp >= as_datetime("2023-02-14")),
               show.legend = FALSE) +
  scale_color_manual(values = MetBrewer::met.brewer("Klimt", 2))
  