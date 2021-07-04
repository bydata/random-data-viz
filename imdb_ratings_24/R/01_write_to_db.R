pacman::p_load("tidyverse", "RSQLite", "DBI", "vroom")

# Datasets: https://datasets.imdbws.com/
# Documentation: https://www.imdb.com/interfaces/
# https://db.rstudio.com/databases/sqlite/


# Connect to SQLite database
con <- dbConnect(SQLite(), "data/database/imdb.db")
dbListTables(con)

# Read IMDB csv files
base_path <- "data/imdb_input"

title_basics <- vroom(file.path(base_path, "title.basics.tsv.gz"))
glimpse(title_basics)
dbWriteTable(con, "title_basics", title_basics, overwrite = FALSE)
dbListTables(con)
dbListFields(con, "title_basics")

title_episode <- vroom(file.path(base_path, "title.episode.tsv.gz"))
glimpse(title_episode)
dbWriteTable(con, "title_episode", title_episode, overwrite = FALSE)
dbListTables(con)

title_ratings <- vroom(file.path(base_path, "title.ratings.tsv.gz"))
glimpse(title_ratings)
dbWriteTable(con, "title_ratings", title_ratings, overwrite = FALSE)
dbListTables(con)

if (dbExistsTable(con, "dummy")) {
  dbRemoveTable(con, "dummy")
}
dbListTables(con)

# Close connection
dbDisconnect(con)
