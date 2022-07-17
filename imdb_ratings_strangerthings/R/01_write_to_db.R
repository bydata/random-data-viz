pacman::p_load("tidyverse", "RSQLite", "DBI", "vroom", "here")

base_path <- here("imdb_ratings_strangerthings")

# Datasets: https://datasets.imdbws.com/
# Documentation: https://www.imdb.com/interfaces/
# https://db.rstudio.com/databases/sqlite/

# Connect to SQLite database
con <- dbConnect(SQLite(), here(base_path, "data/database/imdb.db"))
dbListTables(con)

# Read IMDB csv files
title_basics <- vroom(here(base_path, "data/imdb_input", "title.basics.tsv.gz"))
glimpse(title_basics)
dbWriteTable(con, "title_basics", title_basics, overwrite = FALSE)
dbListTables(con)
dbListFields(con, "title_basics")

title_episode <- vroom(here(base_path, "data/imdb_input", "title.episode.tsv.gz"))
glimpse(title_episode)
dbWriteTable(con, "title_episode", title_episode, overwrite = FALSE)
dbListTables(con)

title_ratings <- vroom(here(base_path, "data/imdb_input", "title.ratings.tsv.gz"))
glimpse(title_ratings)
dbWriteTable(con, "title_ratings", title_ratings, overwrite = FALSE)
dbListTables(con)

# Close connection
dbDisconnect(con)
