#' INSPIRATION & code for Voronoi map: 
#' Colin Angus
#' https://github.com/VictimOfMaths/30DayMapChallenge/blob/main/Day3_Polygons.R

library(tidyverse)
library(sf)
library(here)
library(osmdata)
library(ggtext)

base_path <- here("football-clubs-voronoi-de")

# sparql_endpoint <- "https://query.wikidata.org/sparql?query=SELECT%20%3Fclub%20%3FclubLabel%20%3Fvenue%20%3FvenueLabel%20%3Fcoordinates%0AWHERE%0A%7B%0A%09%3Fclub%20wdt%3AP31%20wd%3AQ476028%20.%0A%09%3Fclub%20wdt%3AP115%20%3Fvenue%20.%0A%09%3Fvenue%20wdt%3AP625%20%3Fcoordinates%20.%0A%09SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D"
df <- read_csv(here(base_path, "data", "query.csv"))

clubs <- c(
  "Borussia Dortmund",
  "FC Bayern Munich",
  "VfB Stuttgart",
  "SC Freiburg",
  "SV Werder Bremen",
  "Bayer 04 Leverkusen",
  "FC Schalke 04",
  "FC Augsburg",
  "1. FC Köln",
  "Borussia Mönchengladbach",
  "TSG 1899 Hoffenheim",
  "RB Leipzig",
  "Hertha BSC",
  "1. FC Union Berlin",
  "VfL Wolfsburg",
  "1. FSV Mainz 05",
  "VfL Bochum",
  "Eintracht Frankfurt"
)

club_colors <- c(
  "#FDE100",
  "#DC052D",
  rgb(227, 34, 25, maxColorValue = 255),
  "black",
  "#1D9053",
  "#E32221",
  "#004D9D",
  "#BA3733",
  "#ED1C24", 
  "#000000",
  "#1C63B7",
  "#dadada",
  "#005CA9",
  "#D4011D",
  "#65B32E",
  "#ED1C24",
  "#005CA9",
  "#E1000F"
)
names(club_colors) <- clubs

df_clubs <- subset(df, clubLabel %in% clubs) %>% 
  group_by(clubLabel) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  st_as_sf(wkt = "coordinates", crs = "EPSG:4326")

## GEOMETRIES ==================================================================
## Shapefile Germany
de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
de <- st_transform(de, crs = "EPSG:4326")

# check if both dataset have the same CRS
st_crs(de) == st_crs(df_clubs)

# club icons
icons_folder <- here(base_path, "team_icons")
icons_files <- list.files(icons_folder)
icons_tags <- glue::glue("<img src=\"{here(icons_folder, icons_files)}\" width=30>")
names(icons_tags) <- clubs


## VORONOI TESSELATION =========================================================

# Create Voronoi cells based on club grounds
voronoi <- df_clubs %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# # intersect with Cologne shape
voronoi <- voronoi[unlist(st_intersects(de, voronoi))] %>%
  st_intersection(de) %>% 
  st_as_sf()

voronoi_buli <- st_join(voronoi, df_clubs) %>% 
  inner_join(data.frame(clubLabel = clubs, primary_color = club_colors)) %>% 
  inner_join(data.frame(clubLabel = clubs, icon = icons_tags)) %>% 
  # rename(geometry = 1) %>% 
  st_as_sf


## PLOT ========================================================================

seed <- 4711
set.seed(seed)
voronoi_buli %>%
  ggplot() +
  geom_sf(
    aes(fill = primary_color),
    col = "white", size = 0.75, show.legend = FALSE) +
  geom_sf(data = df_clubs,
          aes(geometry = coordinates), size = 3,
          shape = 21, col = "white", fill = "grey12") +
  geom_richtext(
    aes(label = icon, 
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = NA, label.size = 0
  ) +
  scale_fill_identity() +
  labs(
    title = "Was wäre wenn jeder den nächsten<br>Bundesliga-Verein unterstützte?",
    caption = "Shapefile: Natural Earth, Koordinaten: Wikidata. Visualisierung: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Inter") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey87"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_markdown(family = "Oswald", size = 28, angle = 1.5,
                              hjust = 0.5, lineheight = 1.15),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here(base_path, "bundesliga-voronoi-map.png"), dpi = 500, width = 7.5, height = 9)

