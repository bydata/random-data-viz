pacman::p_load("tidyverse", "rvest")

wiki_url <- "https://en.wikipedia.org/wiki/List_of_nuclear_power_stations"
page <- read_html(wiki_url)

# Running
table1 <- html_node(page, css = "#mw-content-text > div.mw-parser-output > table:nth-child(9)") %>% 
  html_table()
# Under construction
table2 <- html_node(page, css = "#mw-content-text > div.mw-parser-output > table:nth-child(13)") %>% 
  html_table()
# Permanently shut down
table3 <- html_node(page, css = "#mw-content-text > div.mw-parser-output > table:nth-child(17)") %>% 
  html_table()

df <- bind_rows(table1, table2, table3, .id = "id")  %>% 
  mutate(status = fct_recode(id, "in service" = "1", 
                                   "under construction" = "2", 
                                   "permanently shut down" = "3"),
         coords = str_extract(Location, "-?\\d{1,3}\\.\\d{1,}; -?\\d{1,3}\\.\\d{1,}"),
         station_name = str_extract(Location, "\\(.+\\)"),
         station_name = str_remove_all(station_name, "[()]")
         ) %>% 
  select(station = `Power station`, country = Country, location = Location, 
         coords, station_name, status) %>% 
  separate(coords, into = c("lat", "lon"), sep = "; ", remove = FALSE) %>% 
  mutate(across(c(lon, lat), as.numeric))

write_rds(df, here::here("nuclear-nice-neighbors", "data", "nuclear_power_plants.rds"))
