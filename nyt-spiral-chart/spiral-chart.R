pacman::p_load("tidyverse", "ggtext", "here", "glue", "lubridate")

dates <- seq(as_date("2020-01-01"), as_date("2022-01-06"), 1)

df <- tibble(
  date = dates,
  cases = rnorm(length(dates), 10000, 20000)
)


# Load data
owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
covid <- read_csv(owid_url)
covid_us <- covid %>% 
  filter(location == "United States") %>% 
  select(location, date, new_cases, new_cases_smoothed) %>% 
  arrange(date) %>% 
  mutate(day_of_year = yday(date),
         year = year(date),
         row = row_number())


df <- df %>% 
  arrange(date) %>% 
  mutate(cases = ifelse(cases < 0, 0, round(cases)),
         day_of_year = yday(date),
         year = year(date),
         row = row_number()) 

df %>% 
  ggplot(aes(day_of_year, as_datetime(date))) +
  geom_point(aes(col = factor(year), group = 1, size = cases), 
             # size = 0.1, 
             show.legend = FALSE) +
  scale_x_continuous(n.breaks = 12) +
  scale_size_area() +
  coord_polar() +
  theme_void() +
  theme(
    panel.grid.major.x = element_line(color = "grey50", size = 0.2, linetype = "dotted")
  )


# p <- 
  df %>% 
  ggplot(aes(day_of_year, as_datetime(date))) +
  geom_point(aes(col = factor(year), group = 1, #size = cases, 
                 alpha = row), 
             size = 10,
             shape = "-",
             show.legend = FALSE) +
  scale_x_continuous(n.breaks = 12) +
  scale_size_area() +
  coord_polar() +
  theme_void() +
  theme(
    panel.grid.major.x = element_line(color = "grey50", size = 0.2, linetype = "dotted")
  )
  

df %>% 
  ggplot(aes(x = day_of_year, xend = day_of_year + 1, 
             y = as.POSIXct(date), yend = as.POSIXct(date))) +
  # basic line
  geom_segment() +
  # area
  geom_segment(aes(y = as.POSIXct(date) - cases / 2 * 86400,
                   yend = as.POSIXct(lead(date)) - lead(cases) / 2 * 86400,
                   col = factor(year)),
               # col = "red"
               ) +
  geom_segment(aes(y = as.POSIXct(date) + cases / 2 * 86400,
                   yend = as.POSIXct(lead(date)) + lead(cases) / 2 * 86400),
               col = "red") +
  scale_x_continuous(n.breaks = 12) +
  coord_polar() +
  theme_void() +
  theme(
    panel.grid.major.x = element_line(color = "grey50", size = 0.2, linetype = "dotted")
  )

# with geom_ribbon
size_factor <- 50

covid_us %>% 
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_cases_smoothed / 2 * size_factor,
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor,
                  fill = factor(year)),
              alpha = 0.6,
              show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = "green") +
  scale_x_continuous(n.breaks = 12) +
  coord_polar() +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey50", size = 0.2, linetype = "dotted")
  )

ggsave(here("nyt-spiral-chart", "plots", "nyt_spiral_geom_ribbon.png"),
       dpi = 300, width = 5, height = 5)
  
  

gg <- ggplot_build(p)

layer_data(p) %>% 
  ggplot(aes(x = x, y = y, xend = lead(x), yend = lead(y))) +
  geom_segment() +
  coord_polar(theta = "x") +
  theme_void()



# Accidentally Rtistry
ragg::agg_png(here("nyt-spiral-chart", "plots", "spiral_rtistry.png"), 
              res = 300, width = 800, height = 800)
layer_data(p) %>% 
  ggplot(aes(x = x, y = y, xend = lead(x), yend = lead(y))) +
  ggfx::with_outer_glow(
    geom_segment(color = "deeppink", size = 1),
    sigma = 5, expand = 3, colour = "deeppink" #colorspace::lighten("deeppink", 0.1)
    )+
  coord_polar() +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "grey2")
  )
invisible(dev.off())

a <- 10
b <- 10
df <- tibble(
  x = seq(1, 700, 0.1),
  y = a + b * x)

df %>% 
  ggplot(aes(x, y)) +
  geom_line() +
  coord_polar()


t <- seq(0, 100, by=0.01)
x = t*cos(t); y = t*sin(t)
ggplot(data.frame(x,y), aes(x=x,y=y))+geom_path()
