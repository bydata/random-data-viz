### RUN SCRIPT voronoi-buli1+2.R FIRST ###

base_path <- here("football-clubs-voronoi-de")

#' https://www.destatis.de/DE/Service/EXDAT/Datensaetze/bevoelkerung-geo-mobilfunkdaten.html
#' https://www.destatis.de/static/DE/dokumente/Bevoelkerungsraster_1km_2020.zip
#' CRS: https://epsg.io/3035

raster <- readxl::read_xlsx(here(base_path, "data", "Bevoelkerungsraster (1 km x 1 km).xlsx"), 
                  sheet = 2)


raster_prep <- raster %>% 
  transmute(lat = str_extract(`Gitter-ID`, "[SN]\\d{7}"),
            lat = str_remove(lat, "[SN]") %>% as.numeric(),
            lon = str_extract(`Gitter-ID`, "[EW]\\d{7}"),
            lon = str_remove(lon, "[EW]") %>% as.numeric(),
            Gemeinde,
            pop = ifelse(Exp_georef_BFS_20 == "[0-3]", 1.5, as.numeric(Exp_georef_BFS_20))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:3035") %>% 
  st_transform(crs = "EPSG:4839")
st_crs(raster_prep)


voronoi_buli_raster <- st_join(raster_prep, voronoi_buli, join = st_within)
table(voronoi_buli_raster$clubLabel, useNA = "ifany")

voronoi_buli_raster %>% 
  st_drop_geometry() %>% 
  count(clubLabel, wt = pop, sort = TRUE) 

library(cartogram)

voronoi_buli_raster_agg <- voronoi_buli_raster %>% 
  count(clubLabel, primary_color, icon, wt = pop, sort = TRUE) %>% 
  na.omit()

cartogram_dorling(voronoi_buli_raster_agg, "n", k = 5) %>% 
  ggplot() +
  geom_sf(aes(fill = primary_color), col = "white") +
  geom_richtext(
    aes(label = icon, 
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = NA, label.size = 0
  ) +
  scale_fill_identity() +
  labs(
    title = "Was wäre wenn jeder den nächsten<br>Verein in der 1. oder 2. Bundesliga unterstützte?",
    subtitle = "Die Größte der Kreise richtet sich nach der Einwohnerzahl im 
    \"Einzugsgebiet\" - jener Fläche, in der jeder Punkt näher zum Stadion des 
    jeweiligen Vereins gelegen ist als zu allen anderen Stadien",
    caption = "Shapefile: Natural Earth, Koordinaten: Wikidata,
    Experimentelle georeferenzierte Bevölkerungszahl auf Basis der 
    Bevölkerungsfortschreibung und Mobilfunkdaten, Destatis.
    Visualisierung: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Inter") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey87"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_markdown(family = "Oswald", size = 28, angle = 1.5,
                                  hjust = 0.5, lineheight = 1.15),
    plot.subtitle = element_textbox(hjust = 0.5, width = 0.97, lineheight = 1.1),
    plot.caption = element_textbox(hjust = 0.5, width = 1.2, lineheight = 1.1)
  )
ggsave(here(base_path, "1 + 2. bundesliga-voronoi-dorling-map.png"), dpi = 500, width = 7.5, height = 9)
