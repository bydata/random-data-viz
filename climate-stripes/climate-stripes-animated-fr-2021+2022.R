pacman::p_load("tidyverse", "ggtext", "glue", "here", "lubridate")

df_21 <- readxl::read_xlsx(here("climate-stripes", "ete2021.xlsx"), 
                           .name_repair = janitor::make_clean_names)
df_22 <- readxl::read_xlsx(here("climate-stripes", "ete2022.xlsx"), 
                           .name_repair = janitor::make_clean_names)
glimpse(df_21)
glimpse(df_22)

df <- bind_rows(df_21, df_22) %>% 
  mutate(year = year(date))

# Custom theme
theme_set(
  theme_void(base_family = "Roboto Condensed", base_size = 9) +
    theme(plot.background = element_rect(color = NA, fill = "black"),
          plot.margin = margin(t = 4, r = 4, l = 4, b = 4),
          text = element_text(color = "grey82"),
          plot.title = element_text(family = "Roboto Condensed Bold", hjust = 0.5,
                                    color = "white", size = 12),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, size = 6),
          strip.text = element_text(
            color = "grey82", face = "bold", size = 10,
            margin = margin(t = 12))
    )
)

# Color palette from https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/
col_strip <- rev(RColorBrewer::brewer.pal(11, "RdBu"))



### ANIMATION ------------------------------------------------------------------
#' The first frame will show the "classic" climate stripes.
#' The second frame is basically a diverging bar chart.
#' For the animation, we create both the stripes and the bars with geom_rect. 

library(gganimate)

# Create a dataset with duplicated values and switch between states
# (either via facet_wrap in a static plot or transition_states in the animation)
p <- bind_rows(df, df, .id = "state") %>% 
  mutate(
    date_num = yday(date),
    ymin = ifelse(state == 1, -1, 0),
    ymax = ifelse(state == 1, 1, tm_normales_1991_2020)) %>% 
  ggplot(aes(xmin = date_num - 0.5, xmax = date_num + 0.5)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = tm_normales_1991_2020), 
            show.legend = FALSE) +
  # scale_fill_gradientn(colors = col_strip) +
  scale_fill_gradient2(low = col_strip[2], midpoint = 0, high = col_strip[10]) +
  facet_wrap(vars(year), nrow = 1) +
  labs(
    title = "Été météorologique - France",
    subtitle = "Écart à la moyenne quotidienne de référence 1991-2020\nde la température moyenne agrégée",
    caption = "Données : Infoclimat.fr\n#ShowYourStripes"
  )

p_anim <- p +
  transition_states(state)
anim <- animate(p_anim, res = 200, width = 1200, height = 600, units = "px", 
                end_pause = 10)
anim_save(here("climate-stripes", "climate-stripes-fr-2021+2022.gif"))



## 2021

p <- bind_rows(df_21, df_21, .id = "state") %>% 
  mutate(
    date_num = yday(date),
    ymin = ifelse(state == 1, -1, 0),
    ymax = ifelse(state == 1, 1, tm_normales_1991_2020)) %>% 
  ggplot(aes(xmin = date_num - 0.5, xmax = date_num + 0.5)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = tm_normales_1991_2020), 
            show.legend = FALSE) +
  # scale_fill_gradientn(colors = col_strip) +
  scale_fill_gradient2(low = col_strip[2], midpoint = 0, high = col_strip[10]) +
  labs(
    title = "Été météorologique 2021 - France",
    subtitle = "Écart à la moyenne quotidienne de référence 1991-2020\nde la température moyenne agrégée",
    caption = "Données : Infoclimat.fr\n#ShowYourStripes"
  )

p_anim <- p +
  transition_states(state)
anim <- animate(p_anim, res = 200, width = 800, height = 600, units = "px", 
                end_pause = 10)
anim_save(here("climate-stripes", "climate-stripes-fr-2021.gif"))


## 2022

p <- bind_rows(df_22, df_22, .id = "state") %>% 
  mutate(
    date_num = yday(date),
    ymin = ifelse(state == 1, -1, 0),
    ymax = ifelse(state == 1, 1, tm_normales_1991_2020)) %>% 
  ggplot(aes(xmin = date_num - 0.5, xmax = date_num + 0.5)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = tm_normales_1991_2020), 
            show.legend = FALSE) +
  # scale_fill_gradientn(colors = col_strip) +
  scale_fill_gradient2(low = col_strip[2], midpoint = 0, high = col_strip[10]) +
  labs(
    title = "Été météorologique 2022 - France",
    subtitle = "Écart à la moyenne quotidienne de référence 1991-2020\nde la température moyenne agrégée",
    caption = "Données : Infoclimat.fr\n#ShowYourStripes"
  )

p_anim <- p +
  transition_states(state)
anim <- animate(p_anim, res = 200, width = 800, height = 600, units = "px", 
                end_pause = 10)
anim_save(here("climate-stripes", "climate-stripes-fr-2022.gif"))




