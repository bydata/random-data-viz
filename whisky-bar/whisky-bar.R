pacman::p_load("tidyverse", "sf", "here", "glue", "ggtext", "osmdata")


alabama <- getbb("Alabama, United States", format_out = "sf_polygon", limit = 1)
st_crs(alabama) <- "EPSG:4326"

bars <- opq("Alabama, United States") %>% 
  add_osm_feature(key = "amenity", value = "bar") %>% 
  osmdata_sf()

bars_alabama <- st_filter(bars$osm_points, alabama, .predicate = st_intersects)


ggplot(alabama) +
  geom_sf() +
  geom_sf(data = bars_alabama,
          color = "darkblue", size = 0.1, alpha = 0.4)



# create a grid of hexagons
grid <- st_make_grid(alabama, n = c(75, 75), square = FALSE) %>% 
  st_as_sf() %>% 
  rename(geometry = x)
# alabama_grid <- st_filter(grid, alabama, .predicate = st_intersects)
alabama_grid <- st_intersection(grid, alabama)
plot(alabama_grid)

# calculate distances to the next bar

distance_mat <- st_distance(alabama_grid, bars_alabama)
dim(distance_mat)

next_bar_id <- map(seq_len(nrow(distance_mat)), ~which.min(distance_mat[.x,]))
next_bar_distance <- map(seq_len(nrow(distance_mat)), ~min(distance_mat[.x,]))

alabama_grid_distances <- alabama_grid %>% 
  add_column(next_bar_id = unlist(next_bar_id),
             next_bar_distance = unlist(next_bar_distance)) 

doors_yellow <- "#D3CA42" 
doors_yellow <- "#fff352"
doors_yellow <- "#f2e750"

ggplot() +
  geom_sf() +
  # geom_sf(data = alabama,
  #         color = "grey80", fill = NA, size = 0.2) +
  geom_sf(data = alabama_grid_distances,
          aes(geometry = geometry, fill = next_bar_distance / 1000),
          col = NA, alpha = 0.6) +
  # scale_fill_gradientn(colors = MetBrewer::met.brewer("Isfahan2"), trans = "pseudo_log") +
  scale_fill_gradient(
    # high = "#122b4d",
    high = "#2e1908",
    low = doors_yellow, trans = "pseudo_log",
    breaks = c(1, 3, 10, 30, 150)) +
  # scale_fill_gradient2(
  #   high = "#122b4d", 
  #   mid = "white",
  #   low = doors_yellow, 
  #   midpoint = 2.5,
  #   trans = "pseudo_log",
  #   breaks = c(1, 3, 10, 30, 150)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = glue("Show me the Way to the next
       <span style='color:{doors_yellow}'>Whisky Bar</span>"),
       subtitle = glue("Distance to the closest bar from each point in 
       <span style='color:{doors_yellow}'>Alabama</span>"),
       caption = "Euclidian distance.<br>
       Data: **OpenStreetMap contributors** |
       Visualization: **Ansgar Wolsing**",
       fill = "Distance in km (log)") +
  coord_sf(xlim = c(-89.5, -84), expand = TRUE) + 
  cowplot::theme_map(font_family = "PT Serif") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8"),
    text = element_text(color = "grey82"),
    plot.title = element_markdown(family = "Permanent Marker", face = "plain",
                                  size = 18, color = "grey99"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(face = "plain", size = 10,
                                     margin = margin(t = 6)),
    plot.caption = element_textbox_simple(size = 7, color = "grey76"),
    legend.position = c(0.5, 0.1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.direction = "horizontal",
    legend.key.height = unit(2.5, "mm"),
    legend.key.width = unit(8, "mm"))

ggsave(here("whisky-bar", "whisky-bar-alabama.png"),
       device = ragg::agg_png,
       dpi = 600, width = 6, height = 6)
 

