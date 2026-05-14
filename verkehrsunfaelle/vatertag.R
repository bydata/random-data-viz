library(tidyverse)
library(ggtext)
library(here)

#' Statistisches Bundesamt
#' https://service.destatis.de/DE/verkehrsunfallkalender/

base_path <- "verkehrsunfaelle"
df <- read_csv2(here(base_path, "Verkehrsunfallkalender_Daten_2024.csv"))

selected_unfallart <- "Unfaelle mit Alkoholeinfluss"

df_prep <- df |> 
  pivot_longer(cols = -Datum, names_to = "unfallart", values_to = "anzahl") |> 
  mutate(
    Datum = dmy(Datum),
    jahr = year(Datum),
    monat = month(Datum, label = TRUE, abbr = TRUE, locale = "de_DE.UTF-8"),
    tag = mday(Datum),
    tag_im_jahr = yday(Datum),
    woche = isoweek(Datum),
    woche = case_when(
      woche >= 52 & as.numeric(monat) == 1 ~ 0,
      woche == 1 & as.numeric(monat) == 12 ~ 53, 
      TRUE ~ woche),
    wochentag = wday(Datum, label = TRUE, abbr = TRUE, 
      locale = "de_DE.UTF-8", week_start = 1),
    anzahl = replace_na(anzahl, 0),
    unfallart = str_replace_all(unfallart, "_", " ")
  ) |> 
  rename(datum = Datum) |> 
  filter(unfallart == selected_unfallart)

# Creates polygons to frame the days of each month in the calendars
month_polygons <- function(days) {
  days <- days |> 
    distinct(datum, monat, woche, wochentag)

  months_info <- days |>
    group_by(monat) |>
    summarize(
      wd_first = as.numeric(wochentag[which.min(datum)]),
      wd_last = as.numeric(wochentag[which.max(datum)]),
      wc_first = woche[which.min(datum)],
      wc_last = woche[which.max(datum)]
    )

  months_info |>
    rowwise() |>
    reframe(
      monat = monat,
      x = c(
        wc_first - 0.5,
        wc_first - 0.5,
        wc_last - 0.5,
        wc_last - 0.5,
        wc_last + 0.5,
        wc_last + 0.5,
        wc_first + 0.5,
        wc_first + 0.5,
        wc_first - 0.5
      ),
      y = c(
        wd_first - 0.5,
        7.5,
        7.5,
        wd_last  + 0.5,
        wd_last  + 0.5,
        0.5,
        0.5,
        wd_first - 0.5,
        wd_first - 0.5
      )
    )
}

df_polygons <- df_prep |>
  group_by(jahr) |>
  group_modify(function(x, y) month_polygons(x)) |>
  ungroup()


# Custom month label x positions
month_labels <- df_prep |>
  group_by(monat) |>
  summarize(x = mean(range(woche)), .groups = "drop")


df_prep |> 
  filter(jahr >= 2020) |> 
  ggplot(aes(x = woche, y = as.numeric(wochentag), fill = anzahl)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_path(
    data = filter(df_polygons, jahr >= 2020),
    aes(x, y, group = paste(jahr, monat)),
    inherit.aes = FALSE,
    color = "grey45", linewidth = 0.33) +
  scale_x_continuous(
    breaks = month_labels$x, labels = month_labels$monat,
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_reverse(
    breaks = 1:7, labels = levels(df_prep$wochentag)
  ) +
  scale_fill_gradient(low = "#FFF6E7", high = "#D91111") +
  coord_equal() +
  facet_wrap(~jahr, ncol = 1, strip.position = "right") +
  guides(
    fill = guide_colorbar(title.position = "top")
  ) +
  labs(
    title = "Vorsicht am Vatertag",
    subtitle = "Tägliche Anzahl der Verkehrsunfälle unter Alkoholeinfluss mit Personenschäden<br>
    in Deutschland 2020-2024",
    caption = "**Daten:** Statistisches Bundesamt. **Visualisierung:** Ansgar Wolsing",
    fill = "Anzahl Verkehrsunfälle"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white", ink = "grey20") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y.left = element_text(size = 6, hjust = 0),
    plot.title = element_markdown(family = "Instrument Sans SemiBold", 
      color = "black", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.4, size = 10, margin = margin(b = 12)),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(3, "mm"),
    strip.text.y.right = element_text(
      family = "Instrument Sans Bold", color = "grey50", 
      angle = 0, size = 12, hjust = 0)
  )
ggsave(here(base_path, "vatertag.png"), width = 6, height = 6)
