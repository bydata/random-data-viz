library(tidyverse)
library(ggtext)
library(here)

#' Source: Destatis
#' Download data from 
#' https://www-genesis.destatis.de/genesis//online?operation=table&code=12411-0010&bypass=true&levelindex=0&levelid=1727636356643#abreadcrumb
#' 1990 & 2023, export as CSV
#' Adaption of Maarten Lambrechts's chart: 
#' https://twitter.com/maartenzam/status/1537705354372558848


base_path <- "population-change"

df <- read_csv2(here(base_path, "12411-0010-DLAND_$F.csv"), 
                skip = 6, col_names = c("bundesland", "pop_t1", "pop_t2"),
                locale = locale(encoding = "ISO-8859-1")) |> 
  slice_head(n = 16)

glimpse(df)

df_change <- df %>% 
  mutate(pop_change_abs = pop_t2 - pop_t1,
         pop_change_rel = pop_change_abs / pop_t1) %>% 
  arrange(pop_change_rel) 


# Custom theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed Light") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.1),
    plot.title = element_text(family = "Roboto Condensed", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      hjust = 0, width = 0.9, margin = margin(t = 2, b = 8)),
    plot.caption = element_markdown(hjust = 0),
    # axis.text.x = element_blank()
    axis.title = element_text(family = "Roboto Condensed", face = "bold", size = 8.5),
    axis.text.x.top = element_text(margin = margin(t = 3, b = -10))
  )
)

annotate_custom_textbox <- function(x, y, label, width = unit(2, "inch"), ...) {
  annotate(GeomTextBox,
           x = x, y = y, label = label, width = width,
           size = 3, hjust = 0, family = "Roboto Condensed Light", 
           box.size = 0, fill = alpha("white", 0.8),
           box.padding = unit(rep(2, 4), "pt")
           )
}


df_plot <- df_change %>% 
  arrange(-pop_change_rel) %>% 
  # calculate the cumulative sum of inhabitants
  mutate(cumsum_pop_t1 = cumsum(pop_t1),
         cumsum_pop_t1_lag = lag(cumsum_pop_t1, default = 0)) 


ragg::agg_png(here(base_path, "population-change-de-1990-2023.png"), units = "in",
              width = 8, height = 6, res = 400)
p <- df_plot %>% 
  ggplot() +
  geom_rect(aes(xmin = cumsum_pop_t1_lag, xmax = cumsum_pop_t1, 
                ymin = 0, ymax = pop_change_rel,
                fill = pop_change_rel >= 0),
            col = "white", linewidth = 0.2) +
  geom_text(
    aes(x = cumsum_pop_t1_lag + pop_t1 / 2,
        y = pop_change_rel + ifelse(pop_change_rel >= 0.03, -0.0015, 0.0015),
        hjust = ifelse(pop_change_rel >= 0.03, 1, 0),
        label = bundesland, col = abs(pop_change_rel) <= 0.03),
    size = 2.25, family = "Roboto Condensed", fontface = "bold") +
  annotate_custom_textbox(
           x = 63e6, y = -0.25, 
           label = "4 of the 5 states losing inhabitants are located on the 
           territory of the former GDR") +
  annotate(GeomCurve,
           x = 66e6, xend = 75e6, y = -0.22, yend = -0.19, curvature = -0.2,
           size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate_custom_textbox(
           x = 55.5e6, y = -0.11, 
           label = "Saarland is the only state on the terrority of West Germany 
           to have a declining population",
           width = 0.22) +
  annotate(GeomCurve,
           x = 59.5e6, xend = 66e6, y = -0.05, yend = -0.05, curvature = 0.2,
           size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate_custom_textbox(
           x = 72.5e6, y = 0.06, 
           label = "Brandenburg and (East) Berlin are the only states 
           on the territory of the former 
           GDR to have slightly increased its population") +
  annotate(GeomCurve,
           x = 70e6, xend = 66e6, y = 0.055, yend = 0.01, curvature = 0.2,
           size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(GeomCurve,
           x = 67e6, xend = 47.5e6, y = 0.08, yend = 0.08, curvature = -0.1,
           size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate_custom_textbox(
           x = 5e6, y = -0.105, 
           label = "Bayern (Bavaria) had the largest increase in population, both in 
           relative (17.3%) and absolute terms (1.99 million).",
           width = 0.2) +
  annotate(GeomCurve,
           x = 4e6, xend = 4e6, y = -0.02, yend = -0.0025, curvature = 0.05,
           size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  coord_flip() +
  guides(fill = "none",
         color = "none") +
  labs(
    # title = "Population Change in German Federal States since the Reunification",
    # subtitle = "Total population in 1990 (widths) multiplied by population change until 2023 (heights) gives
    # growth in absolute numbers (surface area of the rectangles). 
    # The width of each rectangle is proportional to the population of
    # the federal states in 1990.",
    caption = "Source: Destatis. Visualization: Ansgar Wolsing",
    x = "Population in 1990 (millions)",
    y = "Population change 1990-2023 (%)"
  ) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_long_scale())) +
  scale_y_continuous(
    labels = scales::label_percent(style_positive = "plus"), breaks = seq(-1, 1, 0.05),
    position = "right") +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("#FF6047", "#488E99"))
p
invisible(dev.off())


## MAP

library(sf)
library(giscoR)
library(patchwork)

shp_de <- giscoR::gisco_get_nuts(country = "Germany", nuts_level = 1)


map <- df_plot |> 
  mutate(bundesland_upper = toupper(bundesland)) |> 
  inner_join(shp_de, by = join_by(bundesland_upper == NUTS_NAME)) |> 
  st_as_sf() |> 
  ggplot() + 
  geom_sf(
    aes(fill = pop_change_rel),
    col = "grey20", linewidth = 0.1
  ) + 
  geom_sf_label(
    aes(label = bundesland),
    family = "Roboto Condensed", size = 1.75, label.size = 0,
    fill = alpha("white", 0.7)
  ) +
  scale_fill_gradient2(low = "#FF6047", high = "#488E99", mid = 0,
                       labels = scales::percent) +
  guides(
    fill = guide_colorbar(
      title = "Relative population change (%)",
      title.position = "top")) +
  theme_void(base_family = "Roboto Condensed Light") +
  theme(
    legend.position = "bottom"
  )
map

ragg::agg_png(here(base_path, "population-change-de-1990-2023-with-map.png"), units = "in",
              width = 6, height = 4, scaling = 0.55, res = 300)
p + map + 
  plot_annotation(
    title = "Population Change in German Federal States since the Reunification",
    subtitle = "Total population in 1990 (heights) multiplied by population change until 2023 (widths) gives
    growth in absolute numbers (surface area of the rectangles). 
    The height of each rectangle is proportional to the population of
    the federal states in 1990.")
dev.off()



ragg::agg_png(here(base_path, "population-change-de-1990-2023-with-map-inset.png"), units = "in",
              width = 6, height = 4, scaling = 0.6, res = 300)
p + inset_element(
  map + 
    theme(
      plot.background = element_rect(color = "grey60", fill = "white"),
      plot.margin = margin(l = 20, r = 20),
      legend.key.height = unit(2.5, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6)
      ), 
  left = -0.21, right = 0.55, top = 0.6, bottom = 0.1, align_to = "plot",
  clip = FALSE) + 
  plot_annotation(
    title = "Population Change in German Federal States since the Reunification",
    subtitle = "Total population in 1990 (heights) multiplied by population change until 2023 (widths) gives
    growth in absolute numbers (surface area of the rectangles). 
    The height of each rectangle is proportional to the population of
    the federal states in 1990.")
dev.off()
