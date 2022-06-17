library(tidyverse)
library(here)
library(gganimate)
library(magick)

base_path <- here("government-spending")

#' Download full indicator dataset from
#' https://data.oecd.org/gga/general-government-spending.htm

df_raw <- read_csv(here(base_path, "data", "DP_LIVE_26022022153954965.csv"))
colnames(df_raw) <- tolower(colnames(df_raw))
glimpse(df_raw)

unique(df_raw$measure)

df_raw %>% 
  filter(time >= 1990) %>% 
  group_by(location) %>% 
  summarize(min = min(time), max = max(time)) %>% 
  arrange(min)


countries_since_1996 <- df_raw %>% 
  filter(indicator == "GGEXP" & subject == "TOT" & measure == "PC_GDP") %>% 
  filter(time >= 1996) %>% 
  group_by(location) %>% 
  summarize(min = min(time), max = max(time)) %>% 
  filter(min == 1996) %>% 
  pull(location)

df_1996 <- df_raw %>% 
  filter(indicator == "GGEXP" & subject == "TOT", measure == "PC_GDP")  %>% 
  filter(location != "KOR") %>% 
  filter(time >= 1996 & location %in% countries_since_1996) %>% 
  select(location, time, value)

# For which countries do we miss data from 2020?
df_1996 %>% 
  group_by(location) %>% 
  summarize(max = max(time)) %>% 
  filter(max < 2020)

## Desk research...
#' Source: Aggregated by Statista
#' Hungary: https://tradingeconomics.com/hungary/government-spending-to-gdp
#' Lithuania: https://tradingeconomics.com/lithuania/government-spending-to-gdp

additional_data_2020 <- tribble(
  ~location, ~time, ~value,
  "USA", 2020, 45.45,
  "GBR", 2020, 49.11,
  "DEU", 2020, 50.84,
  "ESP", 2020, 52.27,
  "FRA", 2020, 61.78,
  "GRC", 2020, 60.69,
  "HUN", 2020, 51.60,
  "ISR", 2020, 46.15,
  "ITA", 2020, 57.29,
  # "KOR", 2020, 25.70,
  "LTU", 2020, 43.50,
  "NLD", 2020, 45.36,
)


df_combined <- bind_rows(df_1996, additional_data_2020)




df_combined %>% 
  mutate(highlight = case_when(
    location %in% c("DEU", "USA", "GBR") ~ location,
    TRUE ~ "Other"
  )) %>% 
  ggplot(aes(time, value)) +
  geom_line(aes(group = location,  col = highlight, size = highlight), alpha = 0.7,
            show.legend = FALSE) +
  geom_text(data = . %>%  filter(time == 2020), 
            aes(label = location, col = highlight),
            hjust = 0, nudge_x = 1 / 52, size = 1.6, family = "Roboto", 
            show.legend = FALSE) +
  scale_color_manual(
    values = c("DEU" = "salmon", "GBR" = "blue", "USA" = "darkgreen", "Other" = "grey70")) +
  scale_size_manual(values = c(0.8, 0.8, 0.3, 0.8)) + 
  labs(
    title = "Total Government Spendings as % of GDP"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey89", size = 0.15),
    axis.title = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off")
ggsave(here(base_path, "plots", "govspend.png"), width = 6, height = 5)



length(unique(df_combined$location))


p <- df_combined %>% 
  mutate(highlight = case_when(
    location %in% c("DEU", "USA", "GBR") ~ location,
    TRUE ~ "Other"
  ),
  location = factor(location, levels = c("DEU", setdiff(unique(df_combined$location), "DEU")))) %>% 
  ggplot(aes(time, value)) +
  geom_line(data = . %>% filter(location == "DEU"),
            aes(group = location), alpha = 0.7, col = "steelblue", size = 1,
            show.legend = FALSE) +
  geom_line(data = . %>% filter(location != "DEU"),
            aes(group = location), 
            alpha = 0.7, size = 0.25, col = "grey76",
            show.legend = FALSE) +
  annotate("label", 
           label = c("Germany", "Selected OECD countries"),
           x = c(2000, 2000),
           y = c(52, 30),
           color = c("white", "grey78"),
           fill = c("steelblue", "grey28"), label.r = unit(0.5, "mm"), label.size = 0,
           hjust = 0) + 
  labs(
    title = "Context Matters",
    subtitle = "Total Government Spendings as % of GDP"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey89", size = 0.15),
    axis.title = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off") 


p_anim <- p + transition_layers(layer_length = 1, transition_length = 2)
anim <- animate(p_anim, device = "png", res = 200, duration = 5, width = 6, 
                height = 5, units = "in", end_pause = 20)
anim_save(here(base_path, "plots", "govspend.gif"))




df_combined %>% 
  mutate(highlight = case_when(
    location %in% c("DEU", "USA", "GBR") ~ location,
    TRUE ~ "Other"
  )) %>% 
  ggplot(aes(time, value)) +
  geom_line(aes(group = location,  col = highlight, size = highlight), alpha = 0.7,
            show.legend = FALSE) +
  geom_text(data = . %>%  filter(time == 2020), 
            aes(label = location, col = highlight),
            hjust = 0, nudge_x = 1 / 52, size = 1.6, family = "Roboto", 
            show.legend = FALSE) +
  scale_color_manual(
    values = c("DEU" = "salmon", "GBR" = "blue", "USA" = "darkgreen", "Other" = "grey70")) +
  scale_size_manual(values = c(0.8, 0.8, 0.3, 0.8)) + 
  labs(
    title = "Total Government Spendings as % of GDP"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey89", size = 0.15),
    axis.title = element_blank(),
    plot.title.position = "plot"
  ) +
  coord_cartesian(clip = "off")
ggsave(here(base_path, "plots", "govspend.png"), width = 6, height = 5)





## Only Germany
# p1 <- image_graph(res = 200, width = 6 * 2.54 * 200, height = 5 * 2.54 * 200)
p1 <- image_graph(res = 200, width = 800, height = 600)
df_combined %>% 
  filter(location == "DEU") %>% 
  ggplot(aes(time, value)) +
  geom_line()
dev.off()

p2 <- image_graph(res = 200, width = 800, height = 600)
df_combined %>% 
  filter(location == "DEU") %>% 
  ggplot(aes(time, value)) +
  geom_line() +
  coord_cartesian(ylim = c(24, 62))
dev.off()

p12 <- image_morph(c(p1, p2))
image_write_gif(p12, here(base_path, "plots", "govspend-de-morph.gif"))
