library(tidyverse)
library(elevatr)
library(rayshader)
library(sf)
library(here)

# https://twitter.com/researchremora/status/1418392590244892673

base_path <- here("3d-maps")

# Source: https://opendata.schleswig-holstein.de/dataset/ffh-gebiete
sh <- st_read(here(base_path, "ffh_gebiete_25000_utm32", "ffh_gebiete_25000_utm32.shp"))
cgn <- st_read(here(base_path, "Stadtteil_0", "Stadtteil.shp"))
cgn <- cgn %>% 
  st_combine() %>% 
  st_as_sf()

# tl 55.065363, 8.252185
# tr 55.066515, 8.583936
# br 54.738880, 8.496718
# bl 54.737900, 8.195845

# sylt <- st_crop(
#   st_transform(sh, crs = 4326), 
#   c(xmin = 8.195845, xmax = 8.583936, ymin = 54.737900, ymax = 55.066515))

sylt <- sh %>%
  filter(GEBIETSNR >= "0916-392" & GEBIETSNR <= "1116-391")

sylt %>% 
  ggplot() +
  geom_sf()

cgn %>% 
  ggplot() +
  geom_sf()


elev <- elevatr::get_elev_raster(sylt, z = 12)
elev_sylt <- raster::mask(elev, sylt)
mat <- raster_to_matrix(elev_sylt)

elev <- elevatr::get_elev_raster(cgn, z = 12)
elev_cgn <- raster::mask(elev, cgn)
mat <- raster_to_matrix(elev_cgn)

mat %>%
  # sphere_shade(texture = create_texture("springgreen","darkgreen",
  #                             "turquoise","steelblue3","white")) %>%
  sphere_shade() %>% 
  plot_3d(mat, windowsize = c(1200, 1200), zoom = 0.5, phi = 27.5, theta = 5,
          zscale = 1, background = "#A3BFF4",
          solid = FALSE)

mat %>% 
  sphere_shade(texture = "imhof2") %>% 
  add_water(detect_water(mat, min_area = 400), color = "imhof3") %>%
  plot_3d(mat, windowsize = c(1200, 1200), zoom = 0.4, phi = 23, theta = 3.25,
          zscale = 1, background = "white",
          solid = FALSE)

render_camera()
render_snapshot(here(base_path, "sylt-rendered.png"),
                width = 3000, height = 3000, software_render = TRUE)

# render_highquality(here(base_path, "sylt-rendered.png"), samples = 100, 
#                    width = 2000, height = 4000)


rgl::rgl.clear()
# rgl::rgl.close()

island_volcano = volcano
island_volcano[island_volcano < mean(island_volcano)] = mean(island_volcano)

#Setting a minimum area avoids classifying small flat areas as water:
island_volcano %>%
  sphere_shade(texture="imhof3") %>%
  add_water(detect_water(island_volcano, min_area = 400),color="imhof3") %>%
  plot_3d(island_volcano)
