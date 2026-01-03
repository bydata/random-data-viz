library(tidyverse)
library(rvest)
library(here)

title_id <- "tt4574334"
overview_page_url <- sprintf("https://www.imdb.com/de/title/%s/ratings/?ref_=tt_ov_rat", title_id)
overview_page <- read_html(overview_page_url)

# Extract the ratings from the heatmap
ratings <- overview_page |> 
  html_node(css = "table.fUnism.ratings-heatmap__table") |> 
  html_table() |> 
  mutate(season = row_number()) |> 
  select(season, everything()) |> 
  pivot_longer(cols = -season, names_to = "episode", values_to = "rating") |> 
  filter(!is.na(rating)) |> 
  mutate(
    episode = str_extract(episode, "E\\. (\\d+)", group = 1),
    episode = as.integer(episode)
  )
write_rds(ratings, here("imdb_ratings_strangerthings", "data", "imdb-ratings.rds"))

# Extract the episode URLs
episode_url_selector <- "table.fUnism.ratings-heatmap__table > tbody tr > td > div > a"

episode_relative_urls <- overview_page |>
  html_nodes(css = episode_url_selector) |> 
  html_attr("href")

# Extract the number of user ratings from the episode pages
rating_count_selector <- "div.sc-8e956c5c-0.cfWEab.sc-af040695-1.jppKBB > div > div:nth-child(1) > a > span > div > div > div:nth-child(3)"
rating_counts <- map_int(
  episode_relative_urls,
  function(url) {
    full_url <- paste0("https://www.imdb.com", url)
    page <- read_html(full_url)
    html_node(page, css = rating_count_selector) |> 
      html_text() |> 
      str_remove("\\.") |> 
      as.integer()
  }
)

# Extend ratings data frame with rating counts
ratings_with_counts <- ratings |> 
  add_column(rating_count = rating_counts)

# Save results
write_csv(
  ratings_with_counts, 
  here("imdb_ratings_strangerthings", "data", "stranger_things_imdb_ratings.csv"))


rating_histogram_counts <- map(
  episode_relative_urls,
  function(url) {
    # selector <- "a.histogram-bar-clickable"
    selector <- "script#__NEXT_DATA__"
    raw_rating_regex <- "\\{\\\"rating\\\":(\\d{1,2}),\\\"voteCount\\\":(\\d+)"
    full_url <- paste0("https://www.imdb.com", url)
    page <- read_html(full_url)
    content <- html_nodes(page, css = selector) |> 
      html_text() 
    raw_ratings <- content |> 
      str_extract_all(raw_rating_regex, simplify = TRUE) 
    rating_values <- raw_ratings |> 
      str_extract("\\{\\\"rating\\\":(\\d{1,2}),", group = 1)
    rating_counts <- raw_ratings |> 
      str_extract("\\\"voteCount\\\":(\\d+)", group = 1)
    data.frame(
      episode_url = url, 
      rating = rating_values,
      count = rating_counts)
  }
)
write_rds(rating_histogram_counts, here("imdb_ratings_strangerthings", "data", "rating_histogram_counts.rds"))

map(
  rating_histogram_counts,
  function(x) {
    x |> 
      mutate(across(c(rating, count), as.integer)) |> 
      ggplot() +
      geom_col(aes(rating, count))
  }
)
