pacman::p_load("tidyverse", "tidygraph", "ggraph", "ggtext", "colorspace",
               "ggbeeswarm", "here")


# set locale for month abbreviations in plot
Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

annotations <- data.frame(
  ts = as_datetime(c("2022-05-03 16:58:00"), tz = "Europe/Berlin"),
  y = c(100),
  label = c(
    "Tweet Kevin Kühnert<br>über TheRepublic"
  )
)

tweets_combined %>% 
  mutate(created_at = with_tz(created_at, tzone = "Europe/Berlin"),
         interval = floor_date(created_at, unit = "5 minutes")) %>% 
  count(interval) %>% 
  filter(interval >= as_datetime("2022-05-03 16:00:00", tz = "Europe/Berlin"),
         interval < max(interval)) %>% 
  ggplot(aes(interval, n)) +
  # geom_line(color = "grey99") +
  geom_area(color = "grey99", fill = alpha("white", 0.1)) +
  # geom_point(shape = 21, color = "grey99", fill = "grey8") +
  # geom_point(
  #   data = annotations,
  #   aes(ts, y), color = "red", size = 10,
  #   inherit.aes = FALSE
  # ) +
  # geom_text(
  #   data = annotations,
  #   aes()
  # ) +
  scale_x_datetime() +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  labs(title = "Tweets zur Wahlarena NRW",
       subtitle = "Anzahl der Tweets zu den Hashtags/Begriffen Wahlarena, Wüst, 
       Kutschaty und #LTWNRW22.<br>*5-Minuten-Intervalle*", 
       caption = "**Daten:** Twitter Search API | **Visualisierung:** Ansgar Wolsing",
       x = NULL, y = "# tweets") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(text = element_text(color = "grey80"),
        axis.text = element_text(color = "grey80"),
        plot.background = element_rect(color = NA, fill = "#150330"),
        plot.title = element_text(family = "Roboto Condensed", face = "bold",
                                  size = 20, color = "white"),
        plot.subtitle = element_markdown(lineheight = 1.3, margin = margin(b = 8)),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.3, color = "grey40", linetype = "dotted"),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 8)),
        plot.caption.position = "plot")
ggsave(here("plots", "tweets_timeline.png"), dpi = 300, width = 7, height = 5)




tweets_combined %>% 
  distinct(screen_name, account_created_at, description) %>% 
  arrange(desc(account_created_at))
