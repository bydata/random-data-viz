# remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(tidyverse)
library(ggtext)
library(here)
library(grid)
library(lubridate)
# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

schmelle_url <- "https://fbref.com/en/players/89565ac5/Marcel-Schmelzer"
stats <- fb_player_season_stats(schmelle_url, stat_type = "standard")

# which competitions
count(stats, Comp,wt = MP_Time)

# add the last 2 season in which he did not play
missing_seasons <- expand.grid(
  Season = c("2020-2021", "2021-2022"),
  Comp = c("1. Bundesliga", "DFB-Pokal"),
  MP_Time = 0,
  Min_Time = 0,
  Gls = NA,
  Ast = NA
)

bvb_color_palette <- c("#FDE100", "black", "grey78", "#494949", "white")

df <- stats %>% 
  filter(Squad == "Dortmund") %>% 
  # exclude Supercup and merge UEFA Cup into Europa League, also remove 3rd division
  mutate(Comp = case_when(
    Comp == "2. UEFA Cup" ~ "2. Europa Lg",
    TRUE ~ Comp
  )) %>% 
  filter(!Comp %in% c("DFL-Supercup", "3. 3. Liga")) %>% 
  select(Season, Comp, MP_Time, Min_Time, Gls, Ast) %>% 
  # # add the last 2 season in which he did not play
  # bind_rows(missing_seasons) %>% 
  # generate increment ID for each season
  mutate(Min_Time = replace_na(Min_Time, 0)) %>% 
  arrange(Season) %>% 
  group_by(Season) %>% 
  mutate(Season_id = cur_group_id()) %>% 
  ungroup()

# Minutes played per season
df %>% 
  group_by(Season) %>% 
  summarize(minutes_total = sum(Min_Time))

x_labels <- c(unique(str_remove_all(df$Season, "\\b20"))[seq(1, 10, 3)], "20-21")
x_breaks <- seq(1, 15, 3)

# due to the mirroring effect, we have to display 
# a true length of 2000 to indicate a length of 1000 minutes
custom_legend_x <- 12
custom_legend_y <- 1000 # 1500
custom_legend_yend <- 3000 # 2500


df %>% 
  ggplot(aes(Season_id, Min_Time, fill = Comp)) +
  geom_stream(type = "mirror", bw = 0.7, extra_span = 0.1, n_grid = 2500,
              color = "white", size = 0.25) +
  # add a custom legend for a height reference
  geom_segment(
    aes(x = custom_legend_x, xend = custom_legend_x,
        y = custom_legend_y, yend = custom_legend_yend),
    inherit.aes = FALSE, size = 0.5, color = "grey20") +
  geom_segment(
    data = data.frame(
      x = custom_legend_x - 0.1, xend = custom_legend_x + 0.1,
      y = seq(custom_legend_y, custom_legend_yend, 2 * 250)),
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE, size = 0.3, color = "grey20"
  ) +
  annotate("text", x = 12.25, 
           # y = (custom_legend_y + custom_legend_yend) / 2,
           # y = custom_legend_yend,
           y = c(custom_legend_y, 
                 (custom_legend_y + custom_legend_yend) / 2, 
                 custom_legend_yend),
           label = c("0", "500", "1.000\nSpielminuten"), 
           hjust = 0,
           family = "Raleway", size = 2.5) + 
  scale_x_continuous(limits = c(0, 14), breaks = x_breaks, labels = x_labels,
                     expand = expansion(add = c(1, 0))) +
  scale_fill_manual(values = bvb_color_palette) +
  labs(
    title = "Marcel Schmelzers Karriere beim BVB",
    subtitle = "Einsatzminuten je Wettbewerb",
    caption = "Daten: FBRef, \U007BworldfootballR\U007D R package.
    Visualisierung: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Raleway") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey97"),
    plot.margin = margin(8, 8, 8, 8),
    # legend.position = "bottom",
    legend.position = c(0.9, 0.2),
    plot.title = element_text(hjust = 0.5, family = "Bangers", size = 24),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(1, "mm"),
    axis.text.x = element_text()
  )
ggsave(here("schmelle", "schmelle_stream.png"), dpi = 600, width = 8, height = 6) 

