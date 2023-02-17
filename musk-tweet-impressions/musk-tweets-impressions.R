library(tidyverse)
library(rvest)
library(lubridate)

#' URL pattern to get the tweets for a given time period without replies and without retweets
#' https://twitter.com/search?q=from%3Aelonmusk%20since%3A2022-12-31%20until%3A2023-01-01%20-filter%3Aretweets%20AND%20-filter%3Areplies&src=typed_query&f=live

# path <- "/Users/ansgar/Downloads/tweets-elonmusk/"
path <- "/Users/ansgar/Downloads/tweets-elonmusk/with-replies"
(files <- list.files(path, ".html"))
filenames <- file.path(path, files)

parse_tweets <- function(file) {
  page <- read_html(file)
  
  # parse the timestamps from the source code
  timestamps <- page %>% 
    html_nodes(css = "a time") %>% 
    html_attr("datetime")
  
  # parse span items (every 4th is a view count item)
  impressions <- page %>% 
    html_nodes(css = "div:nth-child(4) > a > div > div > span > span > span") %>% 
    html_text()
  
  # return a data frame with timestamps and impression counts  
  data.frame(
    timestamp = timestamps,
    impressions = clean_impression_count_items(impressions)# ,
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
tweet_impressions <- tweet_impressions %>% 
  arrange(timestamp)

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
         after_superbowl = timestamp >= as_date("2023-02-13"),
         after_superbowl_1d = timestamp >= as_date("2023-02-14")) %>% 
  ggplot(aes(x = after_superbowl_1d, impressions)) +
  geom_boxplot(
    aes(fill = after_superbowl_1d),
    color = "grey40", linewidth = 0.2, width = 0.3, outlier.color = NA, alpha = 0.2,) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = after_superbowl_1d),
    shape = 21, col = "white", size = 2
    ) +
  scale_x_reverse() +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = "#FDFDFD", fill = "#FDFDFD"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.15, color = "grey80"),
    panel.grid.minor.x = element_line(linewidth = 0.08, color = "grey80")
  )
ggsave("em-tweets-impressions-beeswarm.png", width = 5, height = 4)


#' Source: https://www.platformer.news/p/yes-elon-musk-created-a-special-system

