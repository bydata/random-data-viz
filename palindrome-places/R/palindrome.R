library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(osmdata)
library(sf)


## INPUT
#' Set a `country`, this will automate the download and data processing for that 
#' particular country. 
#' However, adjustments might be necessary:
#' a) If a country has offshore areas (e.g. Netherlands, France) you might want to
#'    limit the coordinate system using coord_sf()
#' b) When saving the plot, adapt to the country's shape (TODO)    
#' c) Too many overlaps among text labels (ggrepel)
#' 
#' Note: Not checked for non-Latin-letter names

# which country >>>>>
country <- "Germany"

# where to store the data
data_dir <- here::here("palindrome-places", "data")


# Checks if a string is a palindrome
# Returns a vector of TRUE/FALSE if given a vector
is_palindrome <- function(s) {
  s_lower <- tolower(s)
  s_split <- strsplit(s_lower, split = "")
  s_lower == sapply(s_split, function(x) paste(rev(x), collapse = ""))
}


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
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  download.file(geonames_url, destfile = geonames_localfile_zip)
  geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) %>% 
        filter(Name != "readme.txt")
  unzip(geonames_localfile_zip, exdir = data_dir)
  
  c("filename" = geonames_localfile$Name[1])
}

filename <- download_and_unzip_geonames(country, data_dir = data_dir)
filename

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
  mutate(name2 = forcats::fct_lump_min(name, min = 2))
nrow(places_palindromes)

# load country shape
shp <- rnaturalearth::ne_countries(scale = 10, country = country, 
                                   returnclass = "sf")

# Annotations
plot_titles <- list(
  title = glue::glue("Palindromic place names in {country}"),
  subtitle = "Populated places according to GeoNames.org (feature class \"P\")",
  caption = "**Source:** GeoNames.org, Natural Earth Data | 
       **Visualization:** Ansgar Wolsing"
)

p1 <- ggplot(shp) +
  geom_sf(size = 0.2) +
  geom_point(data = places_palindromes,
             aes(longitude, latitude),
             shape = 21, color = "white", size = 3, fill = "grey12") +
  ggrepel::geom_text_repel(data = places_palindromes,
             aes(longitude, latitude, label = name),
             family = "Helvetica Neue", color = "grey12", size = 3,
             max.overlaps = 20) +
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
ggsave(here::here("palindrome-places", "plots", 
                  glue::glue("palindrome_places_{country}.png")), 
       dpi = 600, width = 6, height = 8)
# adapt width and height to country shape


n_categories <- count(places_palindromes, name2, sort = TRUE) %>% 
  filter(name2 != "Other") %>% 
  nrow()

p2 <- ggplot(shp) +
  geom_sf(size = 0.2) +
  geom_point(data = places_palindromes,
             aes(longitude, latitude, fill = name2),
             shape = 21, color = "white", size = 3, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = places_palindromes,
                           aes(longitude, latitude, label = name, color = name2),
                           family = "Helvetica Neue", 
                           size = 3.5,
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
ggsave(here::here("palindrome-places", "plots", 
                  glue::glue("palindrome_places_{country}-coloured.png")), 
       dpi = 600, width = 6, height = 8)

count(places_palindromes, name, sort = TRUE)

