library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(osmdata)
library(sf)
library(usmap) # map of U.S. with Alaska and Hawaii placed below

# Checks if a string is a palindrome
# Returns a vector of TRUE/FALSE if given a vector
is_palindrome <- function(s) {
  s_lower <- tolower(s)
  s_split <- strsplit(s_lower, split = "")
  s_lower == sapply(s_split, function(x) paste(rev(x), collapse = ""))
}
is_palindrome(c("Rentner", "otto", "anna"))



data_dir <- "palindrome-places"

#' Downloads and unzips country datasets from GeoNames.org
#' http://download.geonames.org/export/dump/
download_and_unzip_geonames <- function(country, 
                                        data_dir) {
  # get country code from English country name
  country_code <- countrycode::countrycode(country,
                                           origin = "country.name",
                                           destination = "iso2c")
  geonames_url <- glue::glue("http://download.geonames.org/export/dump/{country_code}.zip")
  geonames_localfile_zip <- here::here(data_dir, glue::glue("geonames_{tolower(country_code)}.zip"))
  
  download.file(geonames_url, destfile = geonames_localfile_zip)
  geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) %>% 
        filter(Name != "readme.txt")
  unzip(geonames_localfile_zip, exdir = data_dir)
  
  c("filename" = geonames_localfile$Name[1])
}

# which country
country <- "United States of America"
filename <- download_and_unzip_geonames(country, data_dir = data_dir)

places <- read_tsv(here::here(data_dir, filename),
                   col_names = c(
                     "geonameid",
                     "name",
                     "asciiname",
                     "alternatenames",
                     "latitude",
                     "longitude",
                     "feature_class",
                     "feature_code",
                     "country_code",
                     "cc2",
                     "admin1_code",
                     "admin2_code",
                     "admin3_code",
                     "admin4_code",
                     "population",
                     "elevation",
                     "dem",
                     "timezone",
                     "modification_date"
                   ))

# Find all palindromes in the place names column
places_palindromes <- places %>% 
  filter(is_palindrome(name)) %>% 
  filter(feature_class == "P") %>% # city, village etc., see http://www.geonames.org/export/codes.html
  mutate(name2 = forcats::fct_lump_min(name, min = 4))
nrow(places_palindromes)



# load country shape for the US
foo <- us_map("states")

ggplot(us) +
  geom_polygon(aes(x, y, group = group),
               col = "white") +
  cowplot::theme_map()

proj <- usmap_crs()@projargs

# Transform the coordinates to usmap projection
places_palindromes_transformed <- places_palindromes %>% 
  select(longitude, latitude, name, name2) %>% 
  usmap_transform()

n_categories <- count(places_palindromes, name2, sort = TRUE) %>% 
  filter(name2 != "Other") %>% 
  nrow()

# Annotations
plot_titles <- list(
  title = "Palindromic place names in the United States",
  subtitle = "Populated places according to GeoNames.org (feature class \"P\")",
  caption = "**Source:** GeoNames.org, Natural Earth Data | 
       **Visualization:** Ansgar Wolsing"
)

ggplot(us) +
  geom_polygon(aes(x, y, group = group),
               col = "white", fill = "grey78", size = 0.1) +
  geom_point(data = places_palindromes_transformed,
             aes(longitude.1, latitude.1),
             shape = 21, color = "white", size = 2, fill = "grey12") +
  ggrepel::geom_text_repel(data = places_palindromes_transformed,
             aes(longitude.1, latitude.1, label = name),
             family = "Helvetica Neue Light", color = "grey12", size = 2.5,
             max.overlaps = 20) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  coord_sf() +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "steelblue"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "white"),
    plot.caption = element_markdown()
  )
ggsave(here::here("palindrome-places", "plots", "palindrome_places_us.png"), 
       dpi = 600, width = 8, height = 6)

ggplot(us) +
  geom_polygon(aes(x, y, group = group),
               col = "white", fill = "grey78", size = 0.1) +
  geom_point(data = places_palindromes_transformed,
             aes(longitude.1, latitude.1, fill = name2),
             shape = 21, color = "white", size = 2, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = places_palindromes_transformed,
                           aes(longitude.1, latitude.1, label = name, color = name2),
                           family = "Helvetica Neue", # color = "grey12", 
                           size = 2.5, segment.size = 0.2,
                           max.overlaps = 20,
                           show.legend = FALSE) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(n_categories, "Set1"), "grey30"),
                     aesthetics = c("fill", "color")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "steelblue"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "white"),
    plot.caption = element_markdown()
  )
ggsave(here::here("palindrome-places", "plots", "palindrome_places_us-coloured.png"), 
       dpi = 600, width = 8, height = 6)

count(places_palindromes, name, sort = TRUE)

