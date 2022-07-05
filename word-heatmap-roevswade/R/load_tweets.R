pacman::p_load("tidyverse", "rtweet", "lubridate", "here", "glue")

base_path <- "word-heatmap-roevswade"

# load API access information
access_info <- jsonlite::read_json(here(base_path, "twitter_api_auth.json"))

# create an app-only token to raise rate limits (https://rtweet.info/reference/bearer_token.html)
token <- create_token(
  app = access_info$app_name,
  consumer_key = access_info$api_key,
  consumer_secret = access_info$api_secret_key,
  access_token = access_info$access_token,
  access_secret = access_info$access_token_secret
)
btoken = bearer_token(token)
rate_limit(token = btoken)

q <- c("#RoeVsWade")
since <- "2022-06-24"
until <- "2022-06-26"
tweets <- search_tweets(glue("{q} since:{since} until:{until}"),
                        n = 10^6,
                        include_rts = FALSE,  # no retweets to save some disk space
                        with_replies = TRUE,
                        lang = "en",          # only tweets in English
                        retryonratelimit = TRUE,
                        token = btoken)
write_rds(tweets,
          here(base_path, "data", 
               paste0("tweets_", format(Sys.time(), format = "%y%m%d-%H%M%S"), ".rds")),
          compress = "gz")


## READ AND COMBINE ALL TWEET DATAFRAMES =======================================
tweets_files <- list.files(here(base_path, "data"), pattern = "tweets_.*\\.rds")
tweets_combined <- map_dfr(here(base_path, "data", tweets_files), read_rds) %>%
  #' in case of duplicated tweets (status_id),
  #' keep the one with the most likes
  group_by(status_id) %>%
  filter(rank(-favorite_count, ties.method = "first") == 1) %>%
  ungroup()

# Check deduplication
message(paste("CHECK: tweets are unique:" ,
              nrow(tweets_combined) == length(unique(tweets_combined$status_id))))

