library(tidyverse)
library(ggtext)
library(gganimate)
library(here)

#' https://en.wikipedia.org/wiki/All_I_Want_for_Christmas_Is_You#North_America

base_path <- "all-i-want-for-christmas"
local_path <- here(base_path, "billboard.zip")

#' Download the Billboard data from Kaggle
#' Source: https://www.kaggle.com/datasets/ludmin/billboard
#' Set up your API token as describe at https://www.kaggle.com/docs/api#authentication
#' Make sure your authentication file is located at ~/.kaggle/kaggle.json (if you're a Mac user)
kaggle_dataset_url <- "https://www.kaggle.com/api/v1/datasets/download/ludmin/billboard"
download.file(kaggle_dataset_url, destfile = local_path)
unzip(local_path, files = c("hot100.csv", "radio.csv", "digital_songs.csv"), exdir = base_path)

df_hot100 <- read_csv(here(base_path, "hot100.csv"),
                      name_repair = janitor::make_clean_names)
df_radio <- read_csv(here(base_path, "radio.csv"),
                     name_repair = janitor::make_clean_names)
df_digital <- read_csv(here(base_path, "digital_songs.csv"),
                     name_repair = janitor::make_clean_names)

df_hot100_mariah <- df_hot100 |> 
  filter(str_detect(artist, "Mariah Carey"))
df_radio_mariah <- df_radio |> 
  filter(str_detect(artist, "Mariah Carey"))
df_digital_mariah <- df_digital |> 
  filter(str_detect(artist, "Mariah Carey"))

length(unique(df_hot100_mariah$song))
length(unique(df_radio_mariah$song))
length(unique(df_digital_mariah$song))

alliwant <- "All I Want For Christmas Is You"

#' Calculate an id for consecutive weeks a song is in the Hot 100
#' in order to connect week by week, but leave gaps if a song left and re-entered
#' the top 100
df_hot100_mariah <- df_hot100_mariah |> 
  arrange(artist, song, date) |> 
  group_by(artist, song) |> 
  mutate(
    week_diff = as.integer(
      difftime(date, lag(date, n = 1, default = first(date)), units = "weeks")),
    new_streak = ifelse(week_diff > 1, 1, 0),
    streak_id = tolower(sprintf("%s#%s#%02d", 
                         str_sub(artist, 1, 20), str_sub(song, 1, 20),
                         cumsum(new_streak)))
    ) |> 
  ungroup()
  
  
df_hot100_mariah |> 
  mutate(year = year(date)) |> 
  ggplot(aes(date, rank, group = song)) +
  geom_point(
    aes(fill = song == alliwant, color = song == alliwant),
    alpha = 0.8, shape = 21
  ) +
  scale_y_reverse(limits = c(100, 1), expand = c(0, 0)) +
  scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "gold")) +
  scale_fill_manual(values = c("FALSE" = "grey50", "TRUE" = "red")) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(year), nrow = 1) +
  guides(fill = "none", color = "none") +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = "#f1f1e9", fill = "#f1f1e9"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )
  

df_hot100_mariah |> 
  filter(song == alliwant) |> 
  ggplot(aes(date, rank, group = streak_id)) +
  geom_line() +
  geom_point() +
  scale_y_reverse(limits = c(100, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = "#f1f1e9", fill = "#f1f1e9"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()
  )


df_radio_hot100_mariah <- df_hot100_mariah |> 
  select(date, song, artist, rank) |> 
  mutate(type = "Billboard Hot 100") |> 
  bind_rows(df_radio_mariah |> 
              select(date, song, artist, rank) |> 
              mutate(type = "Radio Hot 100")) |> 
  bind_rows(df_digital_mariah |> 
              select(date, song, artist, rank) |> 
              mutate(type = "Hot Digital Songs")) |> 
  mutate(year = year(date)) |> 
  filter(song == alliwant) |> 
  group_by(type, year) |> 
  summarize(peak_position = min(rank), .groups = "drop_last") |>
  mutate(
    time_diff = year - lag(year, n = 1, default = first(year)),
    new_streak = ifelse(time_diff > 1, 1, 0),
    streak_id = sprintf("%s%02d", type, cumsum(new_streak))
  ) |> 
  ungroup() |> 
  # as of Dec 21st, 2024, the song is #1 in the Billboard Hot 100 once more and
  # 11th in the Radio Hot 100
  mutate(peak_position = case_when(
    year == 2024 & type == "Billboard Hot 100" ~ 1, 
    year == 2024 & type == "Radio Hot 100" ~ 11, 
    TRUE ~ peak_position))


# Annotations
annotation_labels <- c(
  rep("The song was never released as a physical single,
    making it not eligible for the Hot 100 before 1998.
  It peaked at no. 12 in the <b>Radio Hot 100</b> in 1995.", 2),
  rep("In January 2000, it entered the <b style='color: gold'>Billboard Hot 100</b> 
  for the 1st time, at no. 83.", 2),
  rep("The song was considered a *recurrent single* and was thus ineligible for
    re-entry on the Billboard Hot 100.<br><br>
      **Digital Sales** showed its popularity, though.", 2),
  rep("Thanks to relaxed rules, the song made it back into the charts in 2012. 
  Since then, the song has been in the 
   <b style='color: gold'>Billboard Hot 100</b> every year.", 2),
  "**Since 2019**, the song has topped the <b style='color: gold'>Billboard Hot 100</b>
  every Christmas season."
)

df_annotations <- data.frame(
  year = c(1994, rep(2000, 2), rep(2004, 2), 
           rep(2012, 2), rep(2019, 2)),
  # use a separate variable for x so that the original annotation does not move
  # with the animation
  x = c(rep(1997, 2), rep(2000, 2), rep(2002, 2), rep(2012, 2), 2013),
  y = c(1, 1, 83, 83, 25, 25, 45, 45, 36),
  label = annotation_labels
)

bg_gradient <- grid::linearGradient(
  c("#e36d7b", "#de3549", "#eb4034"), stops = c(0, 0.1, 1),
  y1 = unit(1, "npc"), y2 = unit(0, "npc"))

p <- df_radio_hot100_mariah |> 
  mutate(type = fct_rev(type)) |> 
  ggplot(aes(year, peak_position, color = type, group = streak_id)) +
  geom_line(
    aes(linetype = type, linewidth = type),
    lineend = "round") +
  geom_point(size = 1.5, show.legend = FALSE) +
  geom_textbox(
    data = df_annotations,
    aes(x, y, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = 1, maxwidth = unit(0.5, "npc"),
    size = 3.5, family = "Fira Sans", box.size = 0, fill = "#ffffff33",
    color = "white"
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, 5)) +
  scale_y_reverse(
    breaks = c(1, seq(10, 100, 10)),
    limits = c(100, 1), expand = expansion(add = c(1, 0.5))) +
  scale_color_manual(values = c(
    "Billboard Hot 100" = "gold", 
    "Hot Digital Songs" = "#ffffffaa", 
    "Radio Hot 100" = "#ffffffaa")) +
  scale_linetype_manual(values = c(
    "Billboard Hot 100" = "solid",
    "Hot Digital Songs" = "dotted", 
    "Radio Hot 100" = "dashed")) +
  scale_linewidth_manual(values = c(
    "Billboard Hot 100" = 1.6, 
    "Hot Digital Songs" = 0.7,
    "Radio Hot 100" = 0.7)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "All I Want For Christmas Is A Number 1 Single",
    subtitle = "How streaming and changing eligibility rules enabled the 
    incredible chart run of Mariah Carey's
    <i>'All I Want For Christmas Is You'</i>.",
    caption = "Source: Billboard (via User ludmin on Kaggle). 
    Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Peak position per year",
    color = NULL, linetype = NULL, linewidth = NULL
  ) +
  theme_minimal(base_family = "Fira Sans", base_size = 11) +
  theme(
    plot.background = element_rect(color = bg_gradient, fill = bg_gradient),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 8, b = 2, l = 4, r = 4),
    legend.position = "top",
    legend.justification = "left",
    legend.key.width = unit(8, "mm"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title = element_markdown(size = 15, family = "Fira Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.95, lineheight = 1.3),
    plot.caption = element_markdown(size = 6)
  )
p

p_anim <- p +
  transition_reveal(year) +
  ease_aes("cubic-in-out")

# Create an mp4
animate(p_anim, 
        end_pause = 20, nframes = 200,
        width = 1200, height = 1200, res = 200,
        renderer = ffmpeg_renderer(format = "mp4"))
anim_save(here(base_path, "all-i-want-for-xmas.mp4"))
