library(tidyverse)
library(sf)
library(rmapshaper) # for detecting inner lines
library(ggtext)
library(here)
# remotes::install_github("ropengov/eurostat")
library(eurostat)

base_path <- here("eurostat-age-map")

#' https://ec.europa.eu/eurostat/databrowser/view/PROJ_19RDBI3/default/table?lang=en&category=proj.proj_19r
df <- get_eurostat("proj_19rdbi3", 
                    filters = list(indic_de = "MEDAGEPOP"), # median age of population
                    time_format = "num", 
                    type = "code"  # get variable codes and labels
)
df <- label_eurostat(df, code = "geo", fix_duplicated = TRUE)

df_prep <- df %>% 
  mutate(country_code = str_sub(geo_code, 1, 2),
         country = countrycode::countrycode(country_code, origin = "iso2c", destination = "country.name"),
         country = ifelse(country_code == "EL", "Greece", country))

# which countries?
unique(df_prep$country_code)
unique(df_prep$country)


# NUTS shapefile =======================
shp <- st_read(here(base_path, "NUTS_RG_10M_2016_3035.shp", "NUTS_RG_10M_2016_3035.shp"))
crs <- st_crs(shp)

extent <- c(xmin = -24, xmax = 45, ymin = 30, ymax = 70)

shp_filtered <- shp %>% 
  st_transform(crs = "EPSG:4326") %>% 
  st_crop(extent) %>% 
  st_transform(crs)


df_prep %>% 
  distinct(country_code) %>% 
  anti_join(shp_filtered, by = c("country_code" = "CNTR_CODE"))

df_prep %>% 
  distinct(geo_code) %>% 
  anti_join(shp_filtered, by = c("geo_code" = "NUTS_ID"))

ggplot(shp_filtered) + 
  geom_sf(aes(fill = MOUNT_TYPE), col = "white")

countries <- rnaturalearth::ne_countries(
  scale = 10, continent = "Europe", returnclass = "sf") %>% 
  bind_rows(
    rnaturalearth::ne_countries(scale = 10, country = "Turkey", returnclass = "sf")
  ) # %>% 
  # st_transform(crs = "EPSG:4326") %>% 
  # st_crop(extent) %>% 
  # st_transform(crs)

countries <- giscoR::gisco_get_countries(
  region = "Europe", year = "2020", epsg = "3035", resolution = "10") %>% 
  bind_rows(
    giscoR::gisco_get_countries(
      country = "Turkey", year = "2020", epsg = "3035", resolution = "10")
  )

# keep only inner borders
inner_borders <- countries %>% 
  ms_innerlines() %>% 
  as_tibble() %>% 
  st_as_sf()

df_prep %>% 
  filter(time %in% c("2020", "2050", "2100")) %>% 
  inner_join(shp, by = c("geo_code" = "NUTS_ID")) %>% 
  st_as_sf() %>% 
  # st_crop()
  ggplot() + 
  geom_sf(data = countries, fill = "grey90", col = NA) +
  geom_sf(aes(geometry = geometry, fill = values), col = NA, size = 0) +
  geom_sf(data = inner_borders, fill = NA, color = "grey90", linewidth = 0.2) +
  scale_fill_viridis_c(option = "G", direction = -1) +
  coord_sf(xlim = c(1791156, 6400000), ylim = c(1385757, 5412200)) +
  facet_wrap(vars(time)) +
  guides(
    fill = guide_colorsteps(title.position = "top", title = "Projected median age")
  ) +
  theme_void(base_family = "Avenir Book") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(rep(4, 4)),
    # legend.position = c(0.1, 0.7),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.height = unit(2, "mm")
  )
# ggsave(here(base_path, "map-projected-medianage-2050.png"), width = 8, height = 7)
ggsave(here(base_path, "map-projected-medianage-2020-2050-2100-rev.png"), width = 12, height = 4)



## Animated map ================================================================

library(gganimate)

unique(df_prep$time)

year_steps <- 2

p <- df_prep %>% 
  inner_join(shp, by = c("geo_code" = "NUTS_ID")) %>% 
  filter(time %% year_steps == 0) %>%
  # filter(time == 2100) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(data = subset(
    countries, NAME_ENGL %in% 
      c("United Kingdom", "Ukraine", "Belarus", "Russian Federation", "Türkiye", 
        "Serbia", "Montenegro", "Kosovo", "Bosnia and Herzegovina", 
        "North Macedonia", "Albania", "Moldova")),
    fill = "grey87", color = "grey87", linewidth = 0) +
  geom_sf(aes(geometry = geometry, fill = values), col = NA, size = 0) +
  geom_sf(data = inner_borders, fill = NA, color = "grey98", linewidth = 0.1) +
  scale_fill_viridis_c(option = "G") +
  coord_sf(xlim = c(1791156, 6400000), ylim = c(1385757, 5412200)) +
  guides(
    fill = guide_colorsteps(title.position = "top", title = "Projected<br>median age")
  ) +
  labs(
    title = "EUROPE IS AGING",
    subtitle = "Median age in {as.integer(frame_time)}",
    caption = "Source: Eurostat (baseline projection). Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Avenir Book") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(rep(4, 4)),
    legend.position = c(0.1, 0.5),
    legend.direction = "vertical",
    legend.key.height = unit(8, "mm"),
    legend.key.width = unit(2, "mm"),
    legend.title = element_markdown(),
    text = element_text(color = "grey20"),
    plot.title = element_text(color = "#008DA9", family = "Avenir Heavy", hjust = 0.5,
                              size = 20),
    plot.subtitle = element_text(family = "Avenir Heavy", hjust = 0.5, 
                                 color = "grey48", size = 14),
    plot.caption = element_text(hjust = 1)
  )

p_anim <- p + transition_time(time) +
  ease_aes("cubic-in-out")
anim <- animate(p_anim, res = 200, width = 1000, height = 1000, 
                start_pause = 8, end_pause = 16)
anim_save(here(base_path, "map-age-projections-2.gif"))
