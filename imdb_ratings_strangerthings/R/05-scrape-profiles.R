library(tidyverse)
library(rvest)
library(here)

season <- 5 
episode <- 1
rating <- 10

#==============================================================================

base_path <- here("imdb_ratings_strangerthings")
path_page_content <- here(base_path, "data", "imdb_review_pages")

rating_padded <- str_pad(rating, width = 2, side = "left", pad = "0")

page <- read_html(here(path_page_content, 
  sprintf("S%sE%s", season, episode),
  sprintf("S%sE%s_R%s.html", season, episode, rating_padded)))

author_a_xpath <- "//a[@data-testid='author-link']"
nodes <- page |> 
  html_nodes(xpath = author_a_xpath)

page_profile_ids <- nodes |> 
  html_attr("href") |> 
  str_remove_all("\\r\\n|=") |>
  str_extract("(ur\\d+)", group = 1)

# Extract review date
review_dates <- page |> 
  html_nodes(css = "li.review-date") |> 
  html_text() 

df_reviews <- tibble(
  season = season, 
  episode = episode,
  rating = rating,
  reviewer_profile_id = page_profile_ids,
  review_date = mdy(review_dates),
)

write_rds(df_reviews, here(base_path, "data", 
  sprintf("reviews_S%sE%s_R%s.rds", season, episode, rating_padded)))


# Scrape user profiles
user_profile_template_url <- "https://www.imdb.com/user/%s/?ref_=tturv_usr_1_t"
page_profile_urls <- sprintf(user_profile_template_url, page_profile_ids)

scrape_profile <- function(url) {
  page <- read_html(url)
  closeAllConnections()

  node <- html_node(page, css = "script#__NEXT_DATA__")
  data <- html_text(node)
  join_date <- str_extract(data, "\\\"joinDate\\\":\\\"(.+?)\\\"", group = 1)
  user_data_counts <- str_extract(
    data,
    "\\\"userDataCounts\\\":\\{(.+?)\\}", 
    group = 1
  )
  review_count = str_extract(user_data_counts, "\\\"reviewCount\\\":(\\d+)", group = 1)
  ratings_count = str_extract(user_data_counts, "\\\"ratingsCount\\\":(\\d+)", group = 1)

  list(
    join_date = join_date, 
    review_count = review_count,
    ratings_count = ratings_count,
    user_data_counts = user_data_counts)
}
scrape_profile_safely <- safely(scrape_profile)


user_profile_data <- map(
  page_profile_urls,
  function(x) {
    Sys.sleep(2)
    scrape_profile_safely(x)
  }
)
write_rds(
  user_profile_data,
  here(base_path, "data", 
    sprintf("user_profile_data_s%se%s_%s.rds", season, episode, rating_padded))
)


showConnections(all = TRUE)

# How many errors?
compact(transpose(user_profile_data)$error)

user_profile_data <- set_names(user_profile_data, page_profile_ids)
write_rds(
  user_profile_data,
  here(base_path, "data", 
    sprintf("user_profile_data_s%se%s_%s.rds", season, episode, rating_padded))
)

# Create dataframe from the resulting list
df_user_profile_data <- bind_rows(transpose(user_profile_data)$result) |> 
  add_column(
    profile_id = page_profile_ids,
    profile_url = page_profile_urls) |> 
  mutate(
    across(c(ratings_count, review_count), as.integer),
    join_datetime = ymd_hms(join_date),
    join_datetime = if_else(is.na(join_datetime),
      ymd_hms(paste0(str_remove(join_date, "Z$"), ":00")),
      join_datetime),
    join_date = as_date(str_sub(join_date, 1, 10))
  ) |> 
  select(profile_id, profile_url, everything())
write_rds(
  df_user_profile_data,
  here(base_path, "data", 
    sprintf("df_user_profile_data_s%se%s_%s.rds", season, episode, rating_padded))
)
