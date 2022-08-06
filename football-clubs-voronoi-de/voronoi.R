#' INSPIRATION: https://github.com/VictimOfMaths/30DayMapChallenge/blob/main/Day3_Polygons.R
#' Colin Angus

library(tidyverse)
library(sf)
library(here)
library(osmdata)
library(ggtext)

base_path <- here("football-clubs-voronoi-de")

# sparql_endpoint <- "https://query.wikidata.org/sparql?query=SELECT%20%3Fclub%20%3FclubLabel%20%3Fvenue%20%3FvenueLabel%20%3Fcoordinates%0AWHERE%0A%7B%0A%09%3Fclub%20wdt%3AP31%20wd%3AQ476028%20.%0A%09%3Fclub%20wdt%3AP115%20%3Fvenue%20.%0A%09%3Fvenue%20wdt%3AP625%20%3Fcoordinates%20.%0A%09SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D"
df <- read_csv(here(base_path, "data", "query.csv"))

bundesliga_clubs <- c(
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

bundesliga_club_colors <- c(
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
  "grey70",
  "#005CA9",
  "#D4011D",
  "#65B32E",
  "#ED1C24",
  "#005CA9",
  "#E1000F"
)
names(bundesliga_club_colors) <- bundesliga_clubs

df_buli <- subset(df, clubLabel %in% bundesliga_clubs) %>% 
  group_by(clubLabel) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  st_as_sf(wkt = "coordinates", crs = "EPSG:4326")

## GEOMETRIES ==================================================================
## Area of Cologne
de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
de <- st_transform(de, crs = "EPSG:4326")

st_crs(de) == st_crs(df_buli)


# club icons
icons_folder <- here(base_path, "team_icons")
icons_files <- list.files(icons_folder)
icons_tags <- glue::glue("<img src=\"{here(icons_folder, icons_files)}\" width=30>")
names(icons_tags) <- bundesliga_clubs


## VORONOI TESSELATION =========================================================

# Create Voronoi cells based on club grounds
voronoi <- df_buli %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# # intersect with Cologne shape
voronoi <- voronoi[unlist(st_intersects(de, voronoi))] %>%
  st_intersection(de) %>% 
  st_as_sf()

voronoi_buli <- st_join(voronoi, df_buli) %>% 
  inner_join(data.frame(clubLabel = bundesliga_clubs, primary_color = bundesliga_club_colors)) %>% 
  inner_join(data.frame(clubLabel = bundesliga_clubs, icon = icons_tags)) %>% 
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
  geom_sf(data = df_buli,
          aes(geometry = coordinates), size = 3,
          shape = 21, col = "white", fill = "grey12") +
  # geom_sf(
  #   aes(geometry = st_centroid(geometry)),
  #   shape = 21, fill = "grey8", color = "white"
  # ) +
  geom_richtext(
    aes(label = icon, 
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = NA, label.size = 0
  ) +
  # ggimage::geom_image(
  #   aes(image = here(icons_folder, icons_files),
  #       x = st_coordinates(st_centroid(geometry))[, "X"],
  #       y = st_coordinates(st_centroid(geometry))[, "Y"]),
  #   size = 0.035
  # ) +
  # colorspace::scale_fill_discrete_sequential(palette = "Grays") +
  # scale_fill_manual(values = bundesliga_club_colors) +
  scale_fill_identity() +
  labs(
    title = "Was wäre wenn jeder den nächsten Bundesliga-Verein unterstützte?",
    caption = "Shapefile: Natural Earth. Visualisierung: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey87"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_textbox(family = "Oswald", size = 28,
                              hjust = 0.5, width = 1, lineheight = 1.15),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here(base_path, "bundesliga-voronoi-map.png"), dpi = 500, width = 7.5, height = 9)

