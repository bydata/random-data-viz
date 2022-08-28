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

#' Eurostat / GEOSTAT
#' https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat

pop_raster <- st_read(here(base_path, "data", "JRC_GRID_2018", "JRC_POPULATION_2018.shp"))
st_crs(pop_raster)

bg_color <- "white"

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
