pacman::p_load("tidyverse", "ggtext", "here", "glue", "lubridate")

# Load data
owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
covid <- read_csv(owid_url)
covid_us <- covid %>% 
  filter(location == "United States") %>% 
  select(date, new_cases, new_cases_smoothed) %>% 
  arrange(date) %>% 
  # Add the dates before the 1st confirmed case
  add_row(date = as_date("2020-01-01"), new_cases = 0, new_cases_smoothed = 0,
          .before = 1) %>% 
  complete(date = seq(min(.$date), max(.$date), by = 1),
           fill = list(new_cases = 0, new_cases_smoothed = 0)) %>% 
  mutate(day_of_year = yday(date),
         year = year(date),
         row = row_number())

# months_abbr <- unique(month(covid_us$date, label = TRUE))
month_length <- c(31, 29, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- vector("integer", 12)
for (i in seq_along(month_length)) {
  if (i > 1) {
    month_breaks[i] <- month_breaks[i - 1] + month_length[i] 
  } else {
    month_breaks[i] <- 1
  }
  
  
}

# with geom_ribbon
size_factor <- 60
outline_color <- "#D97C86"
base_grey <- "grey32"
# base_family <- "Libre Franklin Medium"
base_family <- "Helvetica"
text_color <- rgb(18, 18, 18, maxColorValue = 255)
subtitle_date <- max(covid_us$date) %>% 
  format("%b. %d, %Y")

ragg::agg_png(here("nyt-spiral-chart", "plots", "nyt_spiral_geom_ribbon.png"),
              res = 300, width = 1500, height = 1500)
covid_us %>% 
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_cases_smoothed / 2 * size_factor,
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor,
                  group = year),
              color = outline_color,
              size = 0.3,
              # fill = alpha(outline_color, 0.5),
              fill = "#F2C2C3",
              show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3) +
  
  # annotation: 7d average
  annotate("richtext", 
           label = "7-day<br>average",
           x = 20, y = as.POSIXct("2021-08-01"),
           family = base_family, size = 2, color = text_color,
           label.colour = NA, fill = NA) +
  annotate("segment",
           x = 20, xend = 22.5, 
           y = as.POSIXct("2021-06-01"), yend = as.POSIXct("2021-03-15"),
           color = text_color, size = 0.3) +
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks[c(1, 4, 7, 10)],
                     # labels = month.abb
                     # labels = month.abb[c(1, 4, 7, 10)]
                     labels = c("Jan.", "April", "July", "Oct."),
                     limits = c(1, NA)
                     ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA)) +
  coord_polar() +
  labs(
    subtitle = subtitle_date
  ) +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5),
    text = element_text(color = text_color),
    plot.subtitle = element_text(hjust = 0.5, size = 6)
  )
invisible(dev.off())

# ggsave(here("nyt-spiral-chart", "plots", "nyt_spiral_geom_ribbon.png"),
#        dpi = 300, width = 5, height = 5)
  
  