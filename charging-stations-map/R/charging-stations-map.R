pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata")


base_path <- here("charging-stations-map")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"
crs <- "EPSG:3035 "

# Shape file of Europe
europe <- rnaturalearth::ne_countries(scale = 50, type = "countries", continent = "Europe", returnclass = "sf")
europe <- filter(europe, admin != "Russia") %>% 
  st_crop(xmin = -13, xmax = 42, ymin = 32, ymax = 69) %>% 
  st_transform(crs)
st_crs(europe)

europe %>% 
  ggplot() +
  geom_sf()

# get the charging stations in Europe ==========================================
# Documentation: https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dcharging_station
charging_stations_filepath <- here(base_path, "data", "charging-stations.rds")
if (FALSE) {
  charging_stations <- opq(bbox = st_bbox(europe), timeout = 1800) %>%
    add_osm_feature(key = "amenity", value = "charging_station") %>%
    osmdata_sf()
  write_rds(charging_stations, charging_stations_filepath)
} else {
  charging_stations <- read_rds(charging_stations_filepath)
}

# select relevant columns
charging_stations_points <- charging_stations$osm_points %>%
  select(osm_id, name, brand, operator, network, capacity, fee, charge, opening_hours, 
         # starts_with("socket"), 
         geometry) %>% 
  st_transform(crs)
colnames(charging_stations_points)
st_crs(charging_stations_points)
st_crs(charging_stations_points) == st_crs(europe)

# filter for points inside Europe
charging_stations_points_europe <- st_filter(charging_stations_points, europe)

bg_color <- lighten("#EFD28D", 0.8) %>% desaturate(0.5)
p <- europe %>% 
  ggplot() +
  geom_sf(fill = desaturate("#004777", 0.2), col = "grey82", size = 0.3) + 
  geom_sf(
    data = charging_stations_points_europe,
    size = 0.00005, alpha = 0.25, color = "#00AFB5"
  ) +
  labs(
    title = "Electric Vehicle Charging Stations in Europe",
    subtitle = "Each dot represents a charging station",
    caption = "Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggsave(here(base_path, "plots", "map-charging-stations.png"), dpi = 800, width = 7, height = 8)

# Washington Post map style
bg_color <- "white"
p <- europe %>% 
  ggplot() +
  geom_sf(fill = "#F8F7F7", col = "#E1E1E1", size = 0.4) + 
  geom_sf(
    data = charging_stations_points_europe,
    size = 0.000025, alpha = 0.25, color = "#00AFB5"
  ) +
  labs(
    title = "Electric Vehicle Charging Stations in Europe",
    subtitle = "Each dot represents a charging station",
    caption = "Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggsave(here(base_path, "plots", "map-charging-stations-wp.png"), dpi = 800, 
       width = 7, height = 8)


## Hexagon grid ================================================================

# shape <- select(europe, admin)
shape <- st_union(europe)
initial <- shape
# initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# Create the grid of hexagons
grid <- st_make_grid(target,
                     cellsize = 20000 * sqrt(400 / 346.4),
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # for hex, TRUE for squares
)
# Add index, transform list to dataframe
grid <- st_sf(index = 1:length(lengths(grid)), grid)
plot(grid)

# get the area of one cell of the grid
area_gridcells <- st_area(grid[1, ])
units(area_gridcells) <- "km^2"
area_gridcells
area_gridcells[as.numeric(area_gridcells) > 0]


# We identify the grids that belongs to a entity by assessing the centroid
# cent_grid <- st_centroid(grid)
# cent_merge <- st_join(cent_grid, initial["index_target"], left = FALSE)
# grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# grid_europe <- st_filter(grid_new, europe)
grid_europe <- st_filter(grid, europe)

ggplot(grid_europe) +
  geom_sf(size = 0.1) +
  geom_sf(
    data = europe,
    size = 0.4, fill = NA
  )

st_crs(charging_stations_points_europe) == st_crs(grid_europe)
grid_europe_charging_stations <- st_join(charging_stations_points_europe, grid_europe, join = st_within)

grid_europe_charging_stations_agg <- aggregate(
  mutate(grid_europe_charging_stations, count = 1) %>% 
  select(count, geometry),
  by = list("index" = grid_europe_charging_stations$index),
  FUN = sum,
  na.rm = TRUE,
  do_union = FALSE
)

p <- grid_europe %>% 
  left_join(st_drop_geometry(grid_europe_charging_stations_agg), by = "index") %>% 
  # mutate(count = replace_na(count, 0)) %>% 
  rename(geometry = grid) %>% 
  ggplot() +
  geom_sf(aes(fill = count), size = 0.05, col = "grey90") +
  geom_sf(
    data = europe,
    fill = NA, col = "#E1E1E1", size = 0.4
  ) +
  scale_fill_viridis_c(
    name = "Charging stations\nper cell (log)", option = "plasma",
    trans = "pseudo_log", na.value = darken("#F8F7F7", 0.2),
    breaks = c(10, 50, 100, 300, 500)) +
  guides(fill = guide_colorbar(ticks = TRUE)) +
  labs(
    title = "Electric Vehicle Charging Stations in Europe",
    subtitle = sprintf(
    "Number of charging stations in grid cells of approximately %3.0f square kilometers.<br>
    The number of sockets at a station is not taken into account.",
    as.numeric(area_gridcells)),
    caption = "Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1),
    legend.position = c(0.9, 0.85), 
    legend.key.width = unit(2.5, "mm")
  )
ggsave(here(base_path, "plots", "map-charging-stations-grid.png"), dpi = 800, 
       width = 7, height = 8)
            
