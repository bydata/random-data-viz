library(tidyverse)
library(sf)
library(rmapshaper) # for detecting inner lines
library(ggtext)
library(here)
# remotes::install_github("ropengov/eurostat")
library(eurostat)

base_path <- here("eurostat-marriages")

#' https://ec.europa.eu/eurostat/databrowser/view/PROJ_19RDBI3/default/table?lang=en&category=proj.proj_19r
df <- get_eurostat("demo_nind", 
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

unique(df_prep$indic_de)

df_prep %>%
  filter(indic_de == "Total first marriage rate - females")
df_prep %>%
  filter(indic_de == "Proportion of first marriages - females")
df_prep %>%
  arrange(geo_code) %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(time >= 2019) %>% View()



# NUTS shapefile =======================

# shp <- st_read(here(base_path, "NUTS_RG_10M_2016_3035.shp", "NUTS_RG_10M_2016_3035.shp"))
# crs <- st_crs(shp)
# 
# extent <- c(xmin = -24, xmax = 45, ymin = 30, ymax = 70)
# 
# shp_filtered <- shp %>% 
#   st_transform(crs = "EPSG:4326") %>% 
#   st_crop(extent) %>% 
#   st_transform(crs)
# 
# df_prep %>% 
#   distinct(country_code) %>% 
#   anti_join(shp_filtered, by = c("country_code" = "CNTR_CODE"))
# 
# df_prep %>% 
#   distinct(geo_code) %>% 
#   anti_join(shp_filtered, by = c("geo_code" = "NUTS_ID"))

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


# Deviation in marriage age between women and men
df_prep %>% 
  filter(indic_de %in% c("Mean age at first marriage - females", "Mean age at first marriage - males")) %>% 
  # select latest data per region
  group_by(geo_code) %>% 
  filter(time == max(time)) %>% 
  ungroup() %>% 
  mutate(gender = str_extract(indic_de, "(females|males)")) %>% 
  pivot_wider(id_cols = c("geo_code", "country", "time"), names_from = "gender", values_from = "values") %>% 
  mutate(age_diff_first_marriage = females - males) %>% 
  inner_join(countries, by = c("geo_code" = "CNTR_ID")) %>% 
  st_as_sf() 


## MARRIAGE RATE ===============================================================

df_prep %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(!is.na(values)) %>% 
  ggplot(aes(time, values, group = country)) +
  geom_smooth(span = 0.5)

marriage_rate_change <- df_prep %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(time %in% c(2019, 2020)) %>% 
  add_count(geo_code) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  pivot_wider(id_cols = c("geo_code", "country"), names_from = "time",
              names_prefix = "marriage_rate_", values_from = "values") %>%  
  mutate(marriage_rate_diff_abs = marriage_rate_2020 - marriage_rate_2019,
         marriage_rate_diff_rel = marriage_rate_diff_abs / marriage_rate_2019) %>% 
  inner_join(countries, by = c("geo_code" = "CNTR_ID")) %>% 
  st_as_sf() 

df_prep %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(time %in% c(1960, 2019)) %>% 
  add_count(geo_code) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  pivot_wider(id_cols = c("geo_code", "country"), names_from = "time",
              names_prefix = "marriage_rate_", values_from = "values") %>%  
  mutate(marriage_rate_diff_abs = marriage_rate_2019 - marriage_rate_1960,
         marriage_rate_diff_rel = marriage_rate_diff_abs / marriage_rate_1960) %>% 
  View()


df_prep %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(time %in% c(1960, 2019, 2020)) %>% 
  add_count(geo_code) %>% 
  filter(n == 3) %>% 
  select(-n) %>% 
  pivot_wider(id_cols = c("geo_code", "country"), names_from = "time",
              names_prefix = "marriage_rate_", values_from = "values") %>%  
  mutate(marriage_rate_diff_2019to2020_abs = marriage_rate_2020 - marriage_rate_2019,
         marriage_rate_diff_2019to2020_rel = marriage_rate_diff_2019to2020_abs / marriage_rate_2019,
         marriage_rate_diff_1960to2019_abs = marriage_rate_2019 - marriage_rate_1960,
         marriage_rate_diff_1960to2019_rel = marriage_rate_diff_1960to2019_abs / marriage_rate_1960) %>% 
  select(country, matches("marriage_rate_diff_.*_rel")) %>% 
  pivot_longer(cols = all_of(matches("marriage_rate_diff_.*_rel")), names_to = "period", values_to = "value") %>% 
  # ggplot(aes(country, value, fill = period)) +
  # geom_col(position = "dodge") +
  # coord_flip() +
  ggplot(aes(country, value, col = period)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


df_prep %>% 
  filter(indic_de == "Crude marriage rate") %>% 
  filter(time %in% c(1960, 2019, 2020)) %>% 
  add_count(geo_code) %>% 
  arrange(geo_code) %>% 
  filter(n == 3) %>% 
  select(-n) %>% 
  ggplot(aes(country, values, col = factor(time))) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


marriage_rate_change %>% 
  ggplot() + 
  geom_sf(data = subset(
    countries, NAME_ENGL %in% 
      c("United Kingdom", "Ukraine", "Belarus", "Russian Federation", "Türkiye", 
        "Serbia", "Montenegro", "Kosovo", "Bosnia and Herzegovina", 
        "North Macedonia", "Albania", "Moldova")),
    fill = "grey50", color = "grey50", linewidth = 0) +
  geom_sf(aes(geometry = geometry, fill = marriage_rate_diff_rel), col = NA, size = 0) +
  geom_sf(data = inner_borders, fill = NA, color = "grey90", linewidth = 0.2) +
  # labels
  geom_sf_label(
    aes(label = scales::percent(marriage_rate_diff_rel, style_positive = "plus", accuracy = 0.1)),
    family = "Chivo", color = "grey10", size = 2.25, fill = NA, # alpha("white", 0.2),
    label.size = 0, label.r = unit(0.5, "mm"), label.padding = unit(0.7, "mm")) +
  colorspace::scale_fill_continuous_diverging(
    "Tropic", labels = scales::percent_format(), rev = TRUE) +
  coord_sf(xlim = c(1791156, 6450000), ylim = c(1385757, 5412200)) +
  guides(
    fill = guide_colorsteps(
      title.position = "top", title = "Relative change", 
      override.aes = list(color = "white"))
  ) +
  labs(
    title = "Strong decline in marriage numbers
    amid Coronavirus pandemic",
    subtitle = "Relative change in crude marriage rates 2019 to 2020 
    (marriages per 1,000 inhabitants)",
    caption = "Date: Eurostat. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Avenir Book") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(rep(4, 4)),
    legend.position = c(0.15, 0.5),
    legend.direction = "vertical",
    legend.key.height = unit(8, "mm"),
    legend.key.width = unit(4, "mm"),
    legend.title = element_markdown(size = 9),
    legend.text = element_markdown(size = 7, family = "Chivo"),
    text = element_text(color = "grey20"),
    plot.title = element_text(
      color = "grey2", family = "Avenir Heavy", hjust = 0.5,
      size = 16),
    plot.subtitle = element_textbox(
      family = "Avenir Medium", hjust = 0.5, color = "grey48", size = 12, 
      width = 0.9, halign = 0.5, lineheight = 1.1),
    plot.caption = element_text(hjust = 1)
  )
ggsave(here(base_path, "map-marriage-rate-change.png"), width = 5, height = 5.5)

