library(tidyverse)
library(elevatr)
library(rayshader)
library(sf)
library(here)
library(terra)

# https://twitter.com/researchremora/status/1418392590244892673

base_path <- here("3d-maps")


nl <- rnaturalearth::ne_countries(scale = 10, country = "Netherlands", returnclass = "sf")
# crop
nl <- nl %>% 
  st_crop(st_bbox(c(xmin = 2, xmax = 8, ymin = 49, ymax = 55))) 

nl %>% 
  ggplot() +
  geom_sf()

elev <- elevatr::get_elev_raster(nl, z = 7)
elev_nl <- raster::mask(elev, nl)
mat <- raster_to_matrix(elev_nl)


# custom_texture <- create_texture(
#   "springgreen","darkgreen", "turquoise","steelblue3", "white")
custom_texture <- create_texture(
  "#3498eb","#0a2c47", "#2aaebf","#59a5e3", "white")
custom_texture <- create_texture(
  "#294382", "white", "#9B2B2D", "white", "white")
custom_texture %>%
  plot_map()

#' NL flag colors:
#' blue: #294382
#' red: #9B2B2D


mat %>% 
  # sphere_shade(texture = "imhof2") %>% 
  # sphere_shade(texture = custom_texture) %>% 
  # plot_3d(mat, windowsize = c(1200, 1200), zoom = 0.6, phi = 25.75, theta = 70,
  #         zscale = 30, background = "white",
  #         solid = TRUE, solidcolor = "grey76")
  sphere_shade(texture = custom_texture, colorintensity = 0.5) %>% 
  plot_3d(mat, windowsize = c(1200, 1200), zoom = 0.6, 
          phi = 65.5, theta = -7.5, zscale = 15, background = "white",
          solid = TRUE, solidcolor = "grey60", soliddepth = "auto", 
          solidlinecolor = "grey44")

render_camera()
render_snapshot(here(base_path, "nl-3d.png"),
                width = 2000, height = 2000, software_render = TRUE)

rgl::rgl.clear()
# rgl::rgl.close()


## POPULATION ----------------------------------------------------------------

de_pop <- terra::rast(here(base_path, "deu_pd_2020_1km_UNadj.tif"))
mat_pop <- raster_to_matrix(de_pop)

# custom_texture <- create_texture(
#   "#3498eb","#0a2c47", "#2aaebf","#59a5e3", "white")
# custom_texture <- create_texture(
#   "#3498eb","#0a2c47", "#e7fa5a","#59a5e3", "white")
custom_texture <- create_texture(
  "#3498eb","#13691e", "#e7fa5a","#c7fa5a", "white")
# custom_texture <- create_texture(
#   "#1beef1", "#fb6ff6", "#08b7ee", "#08b7ee", "white")
unclass(MetBrewer::met.brewer("Navajo", 5))
custom_texture <- create_texture(
  "#660d20", "#e59a52", "#edce79", "#e1c59a", "#094568"
)

custom_texture %>%
  plot_map()

zscale <- 80
textalpha <- 0.8
z <- 6000

mat_pop %>% 
  sphere_shade(texture = custom_texture, colorintensity = 0.5) %>% 
  add_shadow(ray_shade(mat_pop, sunaltitude = 30, sunangle = 212)) %>% 
  plot_3d(mat_pop, windowsize = c(1200, 1200), zoom = 0.6, phi = 34.5, theta = -30,
          zscale = zscale, background = "white", fov = 0,
          solid = TRUE, solidcolor = "grey60", soliddepth = -400, 
          solidlinecolor = "grey44")

# Berlin
render_label(mat_pop, x = 900, y = 310, z = z, zscale = zscale,
             text = "BERLIN", textsize = 1.25, linewidth = 1,
             antialias = TRUE, textalpha = textalpha, 
             family = "mono", linecolor = "grey24", textcolor = "grey10"
)
# Hamburg
render_label(mat_pop, x = 480, y = 180, z = z, zscale = zscale,
             text = "HAMBURG", textsize = 1.25, linewidth = 1, textalpha = textalpha, 
             family = "mono", linecolor = "grey24", textcolor = "grey10"
)
# Ruhr Area
render_label(mat_pop, x = 150, y = 450, z = z * 1.33, zscale = zscale,
             text = "RUHR AREA", textsize = 1.25, linewidth = 1, textalpha = textalpha, 
             family = "mono", linecolor = "grey24", textcolor = "grey10"
)
# Munich
render_label(mat_pop, x = 680, y = 830, z = z * 1.25, zscale = zscale,
             text = "MUNICH", textsize = 1.25, linewidth = 1, textalpha = textalpha, 
             family = "mono", linecolor = "grey24", textcolor = "grey10"
)


render_camera()
# theta         phi        zoom         fov 
# -30.3657457  32.2943127   0.6486614  50.0000000 
render_snapshot(here(base_path, "de-3d-pop.png"),
                width = 2400, height = 2400, software_render = TRUE)


phivechalf <- 10 + 70 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull <- c(phivechalf, rev(phivechalf))
thetavec <- 0 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull <- c(zoomvec, rev(zoomvec))

render_movie(filename = "3d-de-pop-movie.mp4", type = "custom", 
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)



# Clear the RGL window
rgl::rgl.clear()


## HI RESOLUTION

# Source: https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates
de_pop_hi_res <- terra::rast(here(base_path, "population_deu_2019-07-01.tif"))

mat_pop_hi_res <- raster_to_matrix(de_pop_hi_res)
# mat_pop_hi_res_reduced <- resize_matrix(mat_pop_hi_res, scale = 0.5)
mat_pop_hi_res_reduced_2 <- resize_matrix(mat_pop_hi_res, scale = 0.1)
write_rds(mat_pop_hi_res_reduced_2, here(base_path, "de-mat_pop_hi_res_reduced_2.rds"), compress = "gz")
mat_pop_hi_res_reduced_2 <- read_rds(here(base_path, "de-mat_pop_hi_res_reduced_2.rds"))

custom_texture <- create_texture(
  "#3498eb","#0a2c47", "#2aaebf","#59a5e3", "white")
custom_texture %>%
  plot_map()

mat_pop_hi_res_reduced_2 %>% 
  sphere_shade(texture = custom_texture, colorintensity = 0.5) %>% 
  plot_3d(mat_pop_hi_res_reduced_2, windowsize = c(1200, 1200), zoom = 0.6, 
          phi = 34.5, theta = 10, zscale = 1, background = "white",
          solid = TRUE, solidcolor = "grey60", soliddepth = "auto", 
          solidlinecolor = "grey44")

render_camera()
render_snapshot(here(base_path, "de-3d-pop-hi-res.png"),
                width = 2400, height = 2400, software_render = TRUE)

rgl::rgl.clear()
