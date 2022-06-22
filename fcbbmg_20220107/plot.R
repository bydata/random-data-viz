pacman::p_load("tidyverse", "ggtext", "here", "glue", "lubridate")

df <- tribble(
  ~name, ~team, ~caps,
  "Ulreich", "FC Bayern", 0,
  "Kimmich", "FC Bayern", 64,
  "Pavard", "FC Bayern", 42,
  "Süle", "FC Bayern", 37,
  "Sabitzer", "FC Bayern", 58,
  "Roca", "FC Bayern", 0,
  "Musiala", "FC Bayern", 9,
  "Gnabry", "FC Bayern", 31,
  "Müller", "FC Bayern", 110,
  "Lewandowski", "FC Bayern", 128,
  "Tillman", "FC Bayern", 0,
  "Sommer", "M'gladbach", 72,
  "Jantschke", "M'gladbach", 0,
  "Elvedi", "M'gladbach", 35,
  "Ginter", "M'gladbach", 46,
  "Netz", "M'gladbach", 0,
  "Koné", "M'gladbach", 0,
  "Kramer", "M'gladbach", 12,
  "Lainer", "M'gladbach", 33,
  "Stindl", "M'gladbach", 11,
  "Embolo", "M'gladbach", 50,
  "Neuhaus", "M'gladbach", 9,
)

df %>% 
  ggplot(aes(team, caps, fill = team)) +
  stat_summary(geom = "point", fun = mean, shape = "-", size = 20) +
  geom_text(data = filter(df, caps >= 40), aes(label = name),
            hjust = 0, nudge_x = 0.06, size = 2, family = "Inter") +
  ggbeeswarm::geom_beeswarm(shape = 21, color = "white", size = 4) +
  annotate("text", x = 1.5, y = 20, label = "Durchschnitt", family = "Inter",
           hjust = 0.5, color = "grey50", size = 4) +
  geom_curve(aes(x = 1.65, xend = 2, y = 16, yend = 22),
             size = 0.2, col = "grey60", 
             arrow = arrow(type = "closed", angle = 15, length = unit(2, "mm"))) + 
  scale_fill_manual(values = c("FC Bayern" = "#DC052D", "M'gladbach" = "grey8")) +
  labs(title = "<span style='color:#DC052D'>Bayern-Startelf</span> mit deutlich mehr Länderspieleinsätzen",
       x = NULL, 
       y = "Anzahl Länderspiele"
       ) +
  theme_minimal(base_family = "Inter") +
  guides(fill = "none") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey99"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, linetype = "dotted"),
    plot.title = element_markdown(),
    plot.title.position = "plot"
  )
ggsave(here("fcbbmg_20220107", "plots", "caps.png"), dpi = 300,
       width = 5, height = 4)


df %>% 
  group_by(team) %>% 
  summarize(sum(caps))
