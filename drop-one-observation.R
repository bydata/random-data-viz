pacman::p_load("tidyverse", "gganimate", "ggtext")

df <- tibble(
  x = c(0.3e+7, 0.455e+7, 0.460e+7, 0.48e+7, 0.5e+7, 0.54e+7, 0.56e+7,
         0.6e+7, 0.62e+7, 0.625e+7, 0.705e+7, 0.72e+7, 1.05e+7),
  y = c(0.0045, 0.012,    0.014,    0.0044,  0.0195, 0.02,    0.007, 
         0.0155, 0.005,   0.0225,        0.0145, 0.0215,   0.032)
)
df
plot(df)
# correlation
with(df, cor(x, y))

# created n copies of the dataframe and assign a group ID
df_extended <- mutate(df, group = 0)
for (i in seq_len(nrow(df))) {
  df_extended <- bind_rows(df_extended, mutate(df, group = i))
}
# drop one case per group
df_extended <- df_extended %>% 
  group_by(group) %>% 
  mutate(observation = row_number()) %>% 
  filter(observation != group) %>% 
  ungroup()


# correlation
with(filter(df_extended, group == 0), cor(x, y))
with(filter(df_extended, group == 13), cor(x, y))

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  scale_x_continuous(breaks = seq(4e6, 1e7, 2e6)) +
  scale_y_continuous(breaks = c(0.01, 0.02, 0.03)) +
  theme_bw()

highlight_color <- colorspace::qualitative_hcl(palette = "Harmonic", n = 2)[1]
highlight_color <- "deeppink"

r <- c(0.72, 0.48)
r_labels <- tibble(
  group = c(0, 13), 
  r = r,
  # label = glue::glue("*R<sup>2</sup>* = {r}")
  label = c(glue::glue("*R<sup>2</sup>* = <span style='color:{highlight_color}'>0.72</span>"),
            "*R<sup>2</sup>* = 0.48"
  )
)

p <- df_extended %>% 
  filter(group %in% c(0, 13)) %>% 
  mutate(highlight = observation == 13) %>% 
  ggplot(aes(x, y)) +
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, color = "steelblue",
              alpha = 0.3) +
  geom_point(aes(color = highlight, size = highlight, fill = highlight),
             shape = 21, show.legend = FALSE) +
  geom_richtext(data = r_labels,
            aes(x = 8e6, 0.03, label = label),
            inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(4e6, 1e7, 2e6)) +
  scale_y_continuous(breaks = c(0.01, 0.02, 0.03)) +
  scale_fill_manual(values = c("FALSE" = "grey50", "TRUE" = highlight_color)) +
  scale_color_manual(values = c("FALSE" = NA, "TRUE" = "white")) +
  scale_size_manual(values = c(1, 3)) +
  coord_cartesian(xlim = c(3e6, 1.06e7), ylim = c(0.00425, 0.032)) +
  labs(
    title = glue::glue("What happens if we drop the
                       <span style='color:{highlight_color}'>outlier</span>?"),
    x = "Cumulative CO<sub>2</sub> Concentration (ppm)",
    y = "Prevalence") +
  theme_bw(base_family = "Raleway", base_size = 9) +
  theme(panel.grid = element_blank(),
        plot.title = element_markdown(family = "Raleway SemiBold", face = "plain"),
        plot.title.position = "plot",
        axis.title.x = element_markdown())

p + facet_wrap(vars(group))

p_anim <- p + transition_states(group)  
animate(p_anim, nframes = 50, device = "ragg_png", res = 200, width = 800, height = 600)
anim_save("drop_one_observation.gif")


## drop another observation

# correlation
with(filter(df_extended, group == 0), cor(x, y))
with(filter(df_extended, group == 13 & observation != 1), cor(x, y))

r <- c(with(filter(df_extended, group == 0), cor(x, y)), 
       with(filter(df_extended, group == 13), cor(x, y)), 
       with(filter(df_extended, group == 13 & observation != 1), cor(x, y)))
r_labels <- tibble(
  group = c(0, 13, 14), 
  r = round(r, 2),
  label = glue::glue("*R<sup>2</sup>* = {r}")
)

p <- df_extended %>% 
  filter(group %in% c(0, 13)) %>% 
  bind_rows(mutate(filter(df_extended, group == 13 & observation != 1), group = 14)) %>% 
  mutate(highlight = observation %in% c(1, 13)) %>% 
  ggplot(aes(x, y)) +
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, color = "steelblue",
              alpha = 0.3) +
  geom_point(aes(color = highlight, size = highlight, fill = highlight),
             shape = 21, show.legend = FALSE) +
  geom_richtext(data = r_labels,
                aes(x = 8e6, 0.03, label = label),
                inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(4e6, 1e7, 2e6)) +
  scale_y_continuous(breaks = c(0.01, 0.02, 0.03)) +
  scale_fill_manual(values = c("FALSE" = "grey50", "TRUE" = highlight_color)) +
  scale_color_manual(values = c("FALSE" = NA, "TRUE" = "white")) +
  scale_size_manual(values = c(1, 3)) +
  coord_cartesian(xlim = c(3e6, 1.06e7), ylim = c(0.00425, 0.032)) +
  labs(
    title = glue::glue("What happens if we drop the
                       <span style='color:{highlight_color}'>outliers</span>?"),
    x = "Cumulative CO<sub>2</sub> Concentration (ppm)",
    y = "Prevalence") +
  theme_bw(base_family = "Raleway", base_size = 9) +
  theme(panel.grid = element_blank(),
        plot.title = element_markdown(family = "Raleway SemiBold", face = "plain"),
        plot.title.position = "plot",
        axis.title.x = element_markdown())

p + facet_wrap(vars(group))

p_anim <- p + transition_states(group)  
animate(p_anim, nframes = 70, device = "ragg_png", res = 200, end_pause = 20,
        width = 800, height = 600)
anim_save("drop_two_observations.gif")



