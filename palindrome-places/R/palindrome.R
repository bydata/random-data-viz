library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(sf)
library(forcats)
library(rnaturalearth)
library(countrycode)
library(ggfx)
library(ggrepel)


## INPUT
#' Set a `country`, this will automate the download and data processing for that 
#' particular country. 
#' However, adjustments might be necessary:
#' a) If a country has areas spread across the globe (e.g. Netherlands, France, 
#'    also the US) you might want to limit the coordinate system using coord_sf()
#' b) When saving the plot, the aspect ratio is determined from the rectangular 
#'    bounding box of the country. Maybe needs some adjustments.
#' c) Too many overlaps among text labels (ggrepel)
#' d) The plot code will fail if there are more categories than avaiable colors 
#'    in the color palette (9 colors). Increase the minimum value to built a category
#'    in the call of fct_lump()
#' 
#' Note: Not checked for non-Latin-letter names

####################### Input parameters ##########################
# which country >>>>>
country <- "Belgium"
# Check for ASCII version (i.e. "è" becomes "e") - TRUE/FALSE >>>>>
check_ascii <- FALSE
# where to store the data
data_dir <- file.path("palindrome-places", "data")
###################################################################


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
  country_code <- countrycode(country,
                                           origin = "country.name",
                                           destination = "iso2c")
  geonames_url <- sprintf("http://download.geonames.org/export/dump/%s.zip", country_code)
  geonames_localfile_zip <- file.path(data_dir, sprintf("geonames_%s.zip", tolower(country_code)))
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  if (!file.exists(geonames_localfile_zip)) {
    download.file(geonames_url, destfile = geonames_localfile_zip)  
  }
  geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) |> 
        filter(Name != "readme.txt")
  unzip(geonames_localfile_zip, exdir = data_dir)
  
  c("filename" = geonames_localfile$Name[1])
}

filename <- download_and_unzip_geonames(country, data_dir = data_dir)
filename

places <- read_tsv(file.path(data_dir, filename),
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
if (check_ascii) {
  places_palindromes <- places |> 
    filter(is_palindrome(name) | is_palindrome(asciiname)) 
} else {
  places_palindromes <- places |> 
    filter(is_palindrome(name))
}
places_palindromes <- places_palindromes |> 
  filter(feature_class == "P") |> # city, village etc., see http://www.geonames.org/export/codes.html
  mutate(name2 = fct_lump_min(name, min = 4)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")
nrow(places_palindromes)

# load country shape
shp <- ne_countries(scale = 10, country = country, 
                                   returnclass = "sf")

# determine a good aspect ratio to save the plot
min_width <- 8
min_height <- 6
bbox <- st_bbox(shp)
mid_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
lon_correction <- cos(mid_lat * pi / 180)

width_height_ratio <- (abs(bbox["xmax"] - bbox["xmin"]) * lon_correction) /
                       abs(bbox["ymax"] - bbox["ymin"])
width_height_ratio <- unname(width_height_ratio)

if (width_height_ratio > 1) {
  width <- width_height_ratio * min_height
  height <- min_height + 1.5
} else {
  width <- min_width
  height <- min_width / width_height_ratio + 1.5
}

# clamp dimensions to maximum values
max_width <- 16
max_height <- 12
scale_factor <- min(max_width / width, max_height / height, 1)
width <- width * scale_factor
height <- height * scale_factor


# Annotations
plot_titles <- list(
  title = sprintf("Palindromic Places in %s", country),
  subtitle = "Place names that are spelled the same way backward as forward.<br>
  (Populated places according to GeoNames.org)",
  caption = "**Source:** GeoNames.org, Natural Earth Data | 
       **Visualization:** Ansgar Wolsing"
)

st_crs(shp)
st_crs(places_palindromes)

p1 <- ggplot(shp) +
  with_shadow(
    geom_sf(size = 0.2, fill = "grey90"),
    x_offset = 8, y_offset = 8, colour = "grey12"
  ) +
  geom_sf(
    data = places_palindromes,
    shape = 21, color = "white", size = 3, fill = "grey12") +
  geom_label_repel(data = places_palindromes,
             aes(geometry = geometry, label = name),
             stat = "sf_coordinates",
             family = "Instrument Sans SemiBold", color = "grey12", size = 3,
             segment.size = 0.5, segment.linetype = "dotted", 
             fill = "#FFFFFF99", label.size = 0,
             max.overlaps = 20) +
  coord_sf(crs = st_crs(shp)) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  theme_void(base_family = "Instrument Sans", base_size = 14) +
  theme(
    plot.background = element_rect(color = NA, fill = "#f0c851"),
    text = element_text(color = "black"),
    plot.title = element_markdown(
      family = "Libre Bodoni", face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(
      hjust = 0.5, lineheight = 1.33),
    plot.caption = element_markdown(hjust = 0.5),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(file.path("palindrome-places", "plots", 
                  sprintf("palindrome_places_%s.png", country)), 
       dpi = 300, width = width, height = height)
