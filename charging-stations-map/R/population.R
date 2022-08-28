pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata")


base_path <- here("charging-stations-map")

## GEOMETRIES ==================================================================

crs <- "EPSG:3035"

# Shape file of Europe
europe <- rnaturalearth::ne_countries(scale = 50, type = "countries", continent = "Europe", returnclass = "sf")
europe <- filter(europe, !admin %in% c("Russia", "Ukraine", "Belarus", "Moldova")) %>% 
  st_crop(xmin = -13, xmax = 42, ymin = 32, ymax = 69) %>% 
  st_transform(crs)
st_crs(europe)

st_bbox(filter(europe, admin == "Netherlands"))

# ggplot(europe) +
#   geom_sf()

# europe_shp_file <- here(base_path, "data", "ref-countries-2020-01m.shp", 
#                         "CNTR_BN_01M_2020_3035.shp", 
#                         "CNTR_BN_01M_2020_3035.shp")
# europe2 <- st_read(europe_shp_file)
# europe2 %>% 
#   # filter(NAME_ENGL == "Netherlands") %>% 
#   # filter(EU_FLAG == "T") %>% 
#   st_crop(xmin = 2632761, xmax = 5956140, ymin = 1427765, ymax = 5378083) %>% 
#   ggplot() +
#   geom_sf()


shape <- st_union(europe)
initial <- shape
# initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)


#' Eurostat / GEOSTAT
#' https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat

pop_raster <- st_read(here(base_path, "data", "JRC_GRID_2018", "JRC_POPULATION_2018.shp"))
st_crs(pop_raster)

# Create the grid of hexagons
grid <- st_make_grid(target,
                     cellsize = 20000 * sqrt(400 / 346.4 / 40),
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # for hex, TRUE for squares
)
# Add index, transform list to dataframe
grid <- st_sf(index = 1:length(lengths(grid)), grid)

# get the area of one cell of the grid
area_gridcells <- st_area(grid[1, ])
units(area_gridcells) <- "km^2"
area_gridcells
area_gridcells[as.numeric(area_gridcells) > 0]

grid_europe <- st_filter(grid, europe)

st_crs(pop_raster) == st_crs(grid_europe)
pop_grid_europe <- st_join(pop_raster, grid_europe, join = st_intersects)
pop_grid_europe2 <- st_join(pop_raster, grid_europe, join = st_covered_by)

# number of cells not matching with europe grid
nrow(pop_grid_europe)
pop_grid_europe %>% 
  st_drop_geometry() %>% 
  select(index) %>% 
  filter(is.na(index)) %>% 
  nrow()

pop_grid_europe %>% 
  st_drop_geometry() %>% 
  count(index, sort = TRUE) %>% 
  head()

foo <- pop_grid_europe2 %>% 
  filter(is.na(index)) 
p <- foo %>% 
  ggplot() +
  geom_sf(aes(fill = TOT_P_2018), col = "white", size = 0.0001)
ggsave(here(base_path, "plots", "foo.png"))
sum(foo$TOT_P_2018)

foo %>% 
  st_drop_geometry() %>% 
  arrange(-TOT_P_2018) %>% 
  head(10)

pop_grid_europe2 %>% 
  filter(!is.na(index)) %>% 
  st_drop_geometry() %>% 
  arrange(-TOT_P_2018) %>% 
  head(10)
pop_grid_europe2 %>% 
  filter(!is.na(index)) %>% 
  st_drop_geometry() %>% 
  summarize(sum(TOT_P_2018))

pop_grid_europe %>% 
  filter(!is.na(index)) %>% 
  st_drop_geometry() %>% 
  summarize(sum(TOT_P_2018))

pop_grid_europe_agg <- aggregate(
  pop_grid_europe %>% 
    select(TOT_P_2018, geometry),
  by = list("index" = pop_grid_europe$index),
  FUN = sum,
  na.rm = TRUE,
  do_union = FALSE
)

save.image(here(base_path, ".RData"))


bg_color <- "white"

p <- grid_europe %>% 
  left_join(st_drop_geometry(pop_grid_europe_agg), by = "index") %>% 
  mutate(TOT_P_2018 = replace_na(TOT_P_2018, 0)) %>% 
  rename(geometry = grid) %>% 
  ggplot() +
  geom_sf(aes(fill = TOT_P_2018), size = 0.001, col = "grey90") +
  geom_sf(
    data = europe,
    fill = NA, col = "#D1D1D1", size = 0.2
  ) +
  scale_fill_viridis_c(
    name = "Number of<br>residents",
    option = "plasma",
    breaks = c(0, 10, 100, 1e3, 1e4, 1e5, 3e5),
    labels = scales::number_format(),
    trans = "pseudo_log", na.value = "#F8F7F7") +
  guides(fill = guide_colorbar(ticks = TRUE)) +
  # guides(fill = guide_colorsteps()) +
  labs(
      title = "Population Density in Europe",
      subtitle = sprintf(
        "Number of residents in grid cells of approximately %3.0f square kilometers.",
        as.numeric(area_gridcells)),
      caption = "Source: GEOSTAT / Eurostat, 2018. Visualisation: Ansgar Wolsing"
    ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1),
    legend.position = c(0.95, 0.54), 
    legend.key.width = unit(2.5, "mm"),
    legend.title = element_markdown(lineheight = 1.1)
  )
ggsave(here(base_path, "plots", "europe-population-grid.png"), dpi = 600, 
       width = 7, height = 8)


pop_raster %>% 
  st_drop_geometry() %>% 
  arrange(-TOT_P_2018) %>% 
  select(TOT_P_2018, Country) %>% 
  head(10)

st_bbox(pop_raster)
st_bbox(grid_europe)
st_crs(pop_raster)
st_crs(grid_europe)

p <- pop_raster %>% 
  st_crop(xmin = 2632761, xmax = 5956140, ymin = 1427765, ymax = 5378083) %>% 
  ggplot() +
  geom_sf(aes(fill = TOT_P_2018, col = TOT_P_2018), size = 0) +
  geom_sf(
    data = europe,
    fill = NA, col = "#E5E5E5", size = 0.2
  ) +
  scale_fill_viridis_c(
    name = "Number of<br>residents",
    option = "plasma",
    breaks = c(0, 10, 100, 1e3, 1e4, 1e5),
    labels = scales::number_format(),
    trans = "pseudo_log", na.value = "#F8F7F7",
    aesthetics = c("fill", "color")) + 
  guides(fill = guide_colorbar(ticks = TRUE)) +
  labs(
    title = "Population Density in Europe",
    subtitle = "Number of residents in grid cells of approximately 1 square kilometer.",
    caption = "Source: GEOSTAT / Eurostat, 2018. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1),
    legend.position = c(0.95, 0.54), 
    legend.key.width = unit(2.5, "mm"),
    legend.title = element_markdown(lineheight = 1.1)
  )
ggsave(here(base_path, "plots", "europe-population-grid-1km.png"), dpi = 800, 
       width = 7, height = 8)


p <- pop_raster %>% 
  st_crop(xmin = 2632761, xmax = 5956140, ymin = 1427765, ymax = 5378083) %>% 
  ggplot() +
  geom_sf(
    data = europe,
    fill = alpha("#0D1688", 0.8), col = "#D1D1D1", size = 0.2  ## fill with extreme color from the plasma palette
  ) +
  geom_sf(aes(fill = TOT_P_2018, col = TOT_P_2018), size = 0) +
  geom_sf(
    data = europe,
    fill = NA, col = "#D1D1D1", size = 0.2
  ) +
  scale_fill_viridis_c(
    name = "Number of<br>residents",
    option = "plasma",
    breaks = c(0, 10, 100, 1e3, 1e4, 1e5),
    labels = scales::number_format(),
    trans = "pseudo_log", na.value = NA,
    aesthetics = c("fill", "color")) + 
  guides(fill = guide_colorbar(ticks = TRUE)) +
  labs(
    title = "Population Density in Europe",
    subtitle = "Number of residents in grid cells of approximately 1 square kilometer.",
    caption = "Source: GEOSTAT / Eurostat, 2018. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1),
    legend.position = c(0.95, 0.54), 
    legend.key.width = unit(2.5, "mm"),
    legend.title = element_markdown(lineheight = 1.1)
  )
ggsave(here(base_path, "plots", "europe-population-grid-1km-2.png"), dpi = 800, 
       width = 7, height = 8)
