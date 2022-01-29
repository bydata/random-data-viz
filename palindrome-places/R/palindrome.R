library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(osmdata)
library(sf)

# Checks if a string is a palindrome
# Returns a vector of TRUE/FALSE if given a vector
is_palindrome <- function(s) {
  s_lower <- tolower(s)
  s_split <- strsplit(s_lower, split = "")
  s_lower == sapply(s_split, function(x) paste(rev(x), collapse = ""))
}
is_palindrome(c("Rentner", "otto", "anna"))


#' GeoNames:
#' http://download.geonames.org/export/dump/
geonames_url <- "http://download.geonames.org/export/dump/DE.zip"
data_dir <- "palindrome-places"
geonames_localfile_zip <- here::here(data_dir, "geonames_de.zip")
download.file(geonames_url, destfile = geonames_localfile_zip)
geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) %>% 
  filter(Name != "readme.txt")
unzip(geonames_localfile_zip, exdir = data_dir)
places <- read_tsv(here::here(data_dir, geonames_localfile$Name[1]),
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
shp <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")

# Annotations
plot_titles <- list(
  title = "Palindrome places in Germany",
  subtitle = "Populated places according to GeoNames.org (feature class \"P\")",
  caption = "**Source:** GeoNames.org, Natural Earth Data | 
       **Visualization:** Ansgar Wolsing"
)

ggplot(shp) +
  geom_sf(size = 0.2) +
  geom_point(data = places_palindromes,
             aes(longitude, latitude),
             shape = 21, color = "white", size = 3, fill = "grey12") +
  ggrepel::geom_text_repel(data = places_palindromes,
             aes(longitude, latitude, label = name),
             family = "Helvetica Neue Light", color = "grey12", size = 3,
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
ggsave(here::here("palindrome-places", "plots", "palindrome_places_de-2.png"), dpi = 600, width = 6, height = 8)

ggplot(shp) +
  geom_sf(size = 0.2) +
  geom_point(data = places_palindromes,
             aes(longitude, latitude, fill = name2),
             shape = 21, color = "white", size = 3, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = places_palindromes,
                           aes(longitude, latitude, label = name, color = name2),
                           family = "Helvetica Neue Light", # color = "grey12", 
                           size = 3.5,
                           max.overlaps = 20,
                           show.legend = FALSE) +
  scale_color_manual(values = c(MetBrewer::met.brewer("Moreau", 7), "grey44"),
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
ggsave(here::here("palindrome-places", "plots", "palindrome_places_de-coloured.png"), dpi = 600, width = 6, height = 8)

