pacman::p_load("tidyverse", "here", "sf", "osmdata", "shadowtext", "ggtext")


## GEOMETRIES ==================================================================

crs <- "EPSG:4326"


countries <- c("Germany", "France", "Netherlands", "Belgium", "Luxembourg", 
               "Poland", "Czechia", "United Kingdom",
               "Denmark", "Sweden", "Norway", "Spain", "Portugal", "Austria",
               "Switzerland", "Italy")

europe <- rnaturalearth::ne_countries(country = countries, scale = 50, returnclass = "sf")
st_crs(europe)
europe <- st_transform(europe, crs)

p <- ggplot(europe) +
  geom_sf() +
  coord_sf(xlim = c(-20, 30), ylim = c(35, 70))
p


# Get locations of nuclear power plants
# power_features <- opq(bbox = st_bbox(europe)) %>%
# power_features <- opq(bbox = c(0, 42, 6, 55)) %>%
power_features <- opq(bbox = c(0, 40, 25, 55)) %>%
  add_osm_feature(key = "plant:source", value = "nuclear") %>%
  osmdata_sf()
write_rds(power_features,
          here("nuclear-nice-neighbors", "data", glue("power_features.rds")), compress = "gz")



ddr <- getbb("Deutsche Demokratische Republik", featuretype = "settlement", format_out = "sf_polygon")

ggplot(ddr) +  
  geom_sf()


p + geom_point(data = power_features$osm_polygons,
               aes(x = st_coordinates(st_centroid(power_features$osm_polygons$geometry))[, "X"], 
                   y = st_coordinates(st_centroid(geometry))[, "Y"]),
            fill = "red", col = "red") +
  # geom_sf_label(data = power_features$osm_polygons,
  #               aes(label = name), size = 2) +
  coord_sf(xlim = c(-20, 30), ylim = c(35, 70))




selected_country_shapes <- europe %>% 
  filter(sovereignt %in% c("Belgium", "Netherlands", "Luxembourg", "France", "Germany")) 

selected_npp <- st_filter(power_features$osm_polygons, selected_country_shapes,
                          .predicate = st_within) %>% 
  add_row(subset(power_features$osm_polygons, 
                 name == "Kernenergiecentrale Borssele"))
  

geom_curve2 <- function(size = 0.3, color = "white", curvature = 0.2, ...) {
  geom_curve(...,
    size = size, color = color, curvature = curvature, 
    arrow = arrow(angle = 20, length = unit(0.2, "cm"))
    )
}

selected_country_shapes %>% 
  ggplot() +
  geom_sf(aes(fill = sovereignt),
          col = "white", size = 0.2,
          show.legend = FALSE) + 
  geom_point(data = selected_npp,
               aes(x = st_coordinates(st_centroid(geometry))[, "X"], 
                   y = st_coordinates(st_centroid(geometry))[, "Y"]),
               size = 3, shape = 21, fill = "grey8", col = "white") +
  
  # Annotation: Nuclear power plant
  annotate("text", label = "Nuclear power plant",
           x = 4.1, y = 49.6, family = "Oswald", color = "white", size = 5) +
  geom_curve2(x = 4.5, xend = 4.75, y = 49.7, yend = 50) +
  geom_curve2(x = 4.5, xend = 6.1, y = 49.45, yend = 49.35) +
  
  # Title
  annotate("shadowtext", 
           # label = "Maybe you don't like your neighbours",
           label = "You don't mind if I put it here, do you?",
           x = -0.2, y = 52.3, 
           hjust = 0,
           angle = 2, size = 7, family = "Oswald", color = "grey22",
           bg.colour = "white") +
  
  scale_fill_manual(values = MetBrewer::met.brewer("Cross")) +
  coord_sf(xlim = c(0.2, 8), ylim = c(49, 52.5)) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  labs(caption = "Source: **OpenStreetMap contributors** | Visualization: **Ansgar Wolsing**") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white"), #alpha("steelblue", 0.75),
    plot.caption = ggtext::element_markdown(color = "grey40", size = 6)
  )
ggsave(here("nuclear-nice-neighbors", "plots", "nuclear-nice-neighbors.png"),
       dpi = 600, width = 5.5, height = 4)




## GERMANY ---------------------------------------------------------

de <-  GADMTools::gadm_sp_loadCountries("DEU", level = 1, basefile="./", simplify = 0.025)
de_sf <- de$spdf %>% st_as_sf()

power_features_de <- opq(bbox = st_bbox(de_sf)) %>%
  add_osm_feature(key = "plant:source", value = "nuclear") %>%
  osmdata_sf()
write_rds(power_features,
          here("nuclear-nice-neighbors", "data", glue("power_features_de.rds")), compress = "gz")


npp_de <- st_filter(power_features_de$osm_polygons, de_sf)

ggplot(de_sf) +
  geom_sf() +
  geom_point(data = npp_de,
             aes(x = st_coordinates(st_centroid(geometry))[, "X"], 
                 y = st_coordinates(st_centroid(geometry))[, "Y"]),
             shape = 21, fill = "red", col = "white") 


## GORLEBEN ======

gorleben <- getbb("Gorleben", format_out = "sf_polygon")
st_crs(gorleben) <- crs

# create shapes of East and West Germany
east_bland <- c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt",
                "Thüringen")

de_east_sf <- de_sf %>%
  filter(NAME_1 %in% east_bland) %>%
  st_union(by_feature = TRUE, is_coverage = TRUE) %>% 
  st_simplify()

de_west_sf <- de_sf %>%
  filter(!NAME_1 %in% east_bland) %>%
  st_union(by_feature = TRUE, is_coverage = TRUE) %>% 
  st_simplify()


de_sf <- de_sf %>% 
  mutate(east = NAME_1 %in% east_bland)


colors <- MetBrewer::met.brewer("Degas", 6)
colors2 <- c(colors[3], colors[5])


ggplot() +
  # geom_sf(data = de_sf,
  #         aes(fill = east),
  #         color = "white", size = 0.2, show.legend = FALSE) +
  geom_sf(data = de_east_sf,
          color = colors[3], size = 1, fill = colors[3]) +
  geom_sf(data = de_west_sf,
          color = colors[5], size = 1, fill = colors[5]) +
  geom_point(data = gorleben,
             aes(x = st_coordinates(st_centroid(geometry))[, "X"], 
                 y = st_coordinates(st_centroid(geometry))[, "Y"]),
             shape = 21, fill = "grey8", col = "white", size = 3) +
  
  # Annotation: Nuclear waste repository
  annotate("richtext",
           label = "<span style='font-size: 14pt'>Gorleben</span><br>
           (interim nuclear waste repository)",
           x = 11.2, y = 53, family = "Oswald", color = "white", size = 3,
           fill = NA, label.colour = NA, hjust = 1, lineheight = 0.6) +
  # geom_curve2(x = 6, xend = 10, y = 52.75, yend = 54) +
  
  # Title
  annotate("shadowtext", 
           label = "Dumping waste in your neighbour's backyard",
           x = 5.65, y = 54.8, 
           hjust = 0, 
           angle = 4, size = 6, family = "Oswald", color = "grey22",
           bg.colour = "white") +
  
  scale_fill_manual(values = MetBrewer::met.brewer("Cross", 3)) +
  coord_sf(xlim = c(6, 13), ylim = c(50, 55)) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  labs(caption = "Source: **OpenStreetMap contributors** | Visualization: **Ansgar Wolsing**") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.caption = ggtext::element_markdown(color = "grey40", size = 6)
  )
ggsave(here("nuclear-nice-neighbors", "plots", "nuclear-nice-neighbors-gorleben.png"),
       dpi = 600, width = 4, height = 5)






## Baltic area ---------------------------------------

df <- read_rds(here("nuclear-nice-neighbors", "data", "nuclear_power_plants.rds"))

countries <- c("Poland", "Czechia", "Slovakia", "Slovenia", "Hungary", "Romania", "Bulgaria",
               "Ukraine", "Belarus", "Lithuania", "Estonia", "Latvia", "Moldova",
               "Finland", "Sweden", "Russia")
east_europe <- rnaturalearth::ne_countries(country = countries, scale = 50, returnclass = "sf")
st_crs(east_europe)
east_europe <- st_transform(east_europe, crs)

ggplot(east_europe) + 
  geom_sf() +
  geom_point(data = df,
             aes(lon, lat),
             col = "red") +
  geom_text(data = df,
             aes(lon, lat, label = country),
             col = "red") +
  coord_sf(xlim = c(13, 40), ylim = c(42, 60))


east_europe %>% 
  filter(sovereignt %in% c("Bulgaria", "Romania")) %>% 
ggplot() + 
  geom_sf(aes(fill = sovereignt), col = "white", size = 0.2,
          show.legend = FALSE) +
  geom_point(data = df,
             aes(lon, lat),
             size = 3, shape = 21, fill = "grey8", col = "white") +
  # Title
  annotate("shadowtext", 
           label = "What you do to me...",
           x = 20, y = 48, 
           hjust = 0, 
           angle = 4, size = 6, family = "Oswald", color = "grey22",
           bg.colour = "white") +
  
  # Country labels
  geom_sf_text(aes(label = sovereignt),
               family = "Oswald", color = "grey22") +
  
  # Annotation: Nuclear power plant
  # annotate("text", label = "Nuclear power plant",
  #          x = 24, y = 43.2, family = "Oswald", color = "white", size = 5,
  #          hjust = 0) +
  # geom_curve2(x = 23.9, xend = 23.75, y = 43.25, yend = 43.6, curvature = -0.2) +
  # geom_curve2(x = 27, xend = 28, y = 43.4, yend = 44.2) +
  annotate("text", label = "Nuclear power plant",
           x = 24, y = 44.8, family = "Oswald", color = "white", size = 5,
           hjust = 0) +
  geom_curve2(x = 23.9, xend = 23.7, y = 44.7, yend = 43.9, curvature = 0.2) +
  geom_curve2(x = 27.4, xend = 28, y = 44.75, yend = 44.45, curvature = -0.3) +
  
  scale_fill_manual(values = rev(MetBrewer::met.brewer("Cross", 2))) +
  coord_sf(xlim = c(20, 30), ylim = c(42, 48)) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  labs(caption = "Source: **OpenStreetMap contributors** | Visualization: **Ansgar Wolsing**") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.caption = ggtext::element_markdown(color = "grey40", size = 6)
  )
ggsave(here("nuclear-nice-neighbors", "plots", "nuclear-nice-neighbors-bg-ro.png"),
       dpi = 600, width = 5, height = 5)



