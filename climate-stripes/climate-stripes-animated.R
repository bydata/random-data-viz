pacman::p_load("tidyverse", "ggtext", "glue", "here")

data_url <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/air_temperature_mean/regional_averages_tm_year.txt"
df_raw <- read_delim(data_url, delim = ";", trim_ws = TRUE,
                 locale = locale(decimal_mark = "."), skip = 1)

# Transform data to long format
df_long <- df_raw %>% 
  select(-2, -`...20`) %>% 
  rename(year = 1) %>% 
  pivot_longer(cols = -year, names_to = "territory", values_to = "avg_temp")

first_year <- min(df_long$year)
last_year <- max(df_long$year)

# Calculate the baseline temperature
# https://en.wikipedia.org/wiki/Warming_stripes#cite_note-ColourChoice-37
baseline_temp <- df_long %>% 
  filter(territory == "Deutschland", year >= 1971, year <= 2000) %>% 
  summarize(baseline_temp = mean(avg_temp)) %>% 
  pull(baseline_temp)

# Color palette from https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/
col_strip <- rev(RColorBrewer::brewer.pal(11, "RdBu"))

# filter for all of Germany, center temperature around the long-term baseline
df_long_centered_de <- df_long %>% 
  filter(territory == "Deutschland") %>% 
  mutate(avg_temp_centered = avg_temp - baseline_temp) 

# Custom theme
theme_set(
  theme_void(base_family = "Roboto Condensed", base_size = 9) +
    theme(plot.background = element_rect(color = NA, fill = "black"),
          plot.margin = margin(t = 4, r = 4, l = 4, b = 4),
          plot.title = element_text(family = "Roboto Condensed Bold", hjust = 0.5,
                                    color = "white", size = 12),
          plot.subtitle = element_text(color = "grey82", hjust = 0.5)
    )
)


# Plot the Climate stripes
df_long_centered_de %>% 
  ggplot(aes(year, y = 1)) +
  geom_tile(aes(fill = avg_temp_centered), show.legend = FALSE) +
  scale_fill_gradientn(colors = col_strip)
ggsave(here("climate-stripes", "climate-stripes-de.png"), dpi = 200, width = 6, height = 2)


### ANIMATION ------------------------------------------------------------------
#' The first frame will show the "classic" climate stripes.
#' The second frame is basically a diverging bar chart.
#' For the animation, we create both the stripes and the bars with geom_rect. 

library(gganimate)

# create the bars with geom_rect
df_long_centered_de %>% 
  ggplot(aes(xmin = year - 0.5, xmax = year + 0.5)) +
  geom_rect(aes(ymin = -1, ymax = 1, fill = avg_temp_centered), show.legend = FALSE) +
  scale_fill_gradientn(colors = col_strip)

df_long_centered_de %>% 
  ggplot(aes(xmin = year - 0.5, xmax = year + 0.5)) +
  geom_rect(aes(ymin = 0, ymax = avg_temp_centered, fill = avg_temp_centered), show.legend = FALSE) +
  scale_fill_gradientn(colors = col_strip) 


# Create a dataset with duplicated values and switch between states
# (either via facet_wrap in a static plot or transition_states in the animation)
p <- bind_rows(df_long_centered_de, df_long_centered_de, .id = "state") %>% 
  mutate(ymin = ifelse(state == 1, -1, 0),
         ymax = ifelse(state == 1, 1, avg_temp_centered)) %>% 
  ggplot(aes(xmin = year - 0.5, xmax = year + 0.5)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = avg_temp_centered), 
            show.legend = FALSE) +
  scale_fill_gradientn(colors = col_strip) +
  labs(
    title = "Climate Stripes Germany",
    subtitle = "#ShowYourStripes"
  )
  #+  facet_wrap(vars(state))

p_anim <- p +
  transition_states(state)
anim <- animate(p_anim, res = 200, width = 800, height = 600, units = "px", 
                end_pause = 10)
anim_save(here("climate-stripes", "climate-stripes-de.gif"))
