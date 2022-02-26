library(tidyverse)
library(here)
library(gganimate)
library(magick)

base_path <- here("government-spending")

## DATA PREPARATION ============================================================

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


## IMAGE ANIMATION =============================================================

# Define common labels and ggplot theme 

labels <- labs(
  title = "Context Matters",
  subtitle = "Total Government Spendings as % of GDP",
  caption = "**Source:** OECD, Statista, tradingeconomics.com | **Visualization:** Ansgar Wolsing"
)

theme <- theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey89", size = 0.15),
    axis.title = element_blank(),
    plot.title.position = "plot",
    text = element_text(color = "grey38"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_markdown(
      hjust = 0, size = 7, margin = margin(t = 8)
    )
  )

coord <- coord_cartesian(ylim = c(24, 62), clip = "off")

res <- 200
image_width <- res * 2.54 * 3
image_height <- res * 2.54 * 2

deu_color <- "steelblue"

## STEP 1: Plot with narrow y-scale ----------

img1 <- image_graph(res = res, width = image_width, height = image_height)
df_combined %>% 
  filter(location == "DEU") %>% 
  ggplot(aes(time, value)) +
  geom_line(color = deu_color) +
  coord_cartesian(clip = "off") +
  labels + 
  theme
dev.off()


## STEP 2: Plot with wider y-scale ----------

img2 <- image_graph(res = res, width = image_width, height = image_height)
df_combined %>% 
  filter(location == "DEU") %>% 
  ggplot(aes(time, value)) +
  geom_line(color = deu_color) +
  coord +
  labels + 
  theme
dev.off()


## STEP 3: Add more countries for context ----------

img3 <- image_graph(res = res, width = image_width, height = image_height)
p3 <- df_combined %>% 
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
           y = c(46, 30),
           color = c("white", "white"), family = "Roboto Condensed",
           fill = c("steelblue", "grey48"), 
           label.r = unit(0.5, "mm"), label.size = 0,
           hjust = 0) + 
  coord + 
  labels +
  theme
p3
dev.off()


## STEP 4: Highlight France for context ----------

img4 <- image_graph(res = res, width = image_width, height = image_height)
p4 <- p3 + 
  geom_line(data = . %>% filter(location == "FRA"),
            aes(group = location), alpha = 0.7, col = "salmon", size = 1,
            show.legend = FALSE) +
  annotate("label", 
           label = "France",
           x = 2000,
           y = 54,
           color = "white", family = "Roboto Condensed",
           fill = "salmon", label.r = unit(0.5, "mm"), label.size = 0,
           hjust = 0) 
p4
dev.off()


## STEP 5: Highlight USA for context ----------

img5 <- image_graph(res = res, width = image_width, height = image_height)
p5 <- p4 + 
  geom_line(data = . %>% filter(location == "USA"),
            aes(group = location), alpha = 0.7, col = "black", size = 1,
            show.legend = FALSE) +
  annotate("label", 
           label = "USA",
           x = 2000,
           y = 39,
           color = "white", family = "Roboto Condensed", 
           fill = "black", label.r = unit(0.5, "mm"), label.size = 0,
           hjust = 0)
p5
dev.off()


## STEP 6: Wait, what happened in 2020? ----------

corona_png_url <- "https://upload.wikimedia.org/wikipedia/commons/8/82/SARS-CoV-2_without_background.png"
corona_png <- png::readPNG(here(base_path, "input", "SARS-CoV-2_without_background.png"))
corona_grob <- grid::rasterGrob(corona_png, interpolate=TRUE)

img6 <- image_graph(res = res, width = image_width, height = image_height)
p6 <- p5 + 
  annotate("text", x = 2018.5, y = 64,
           label = "Wait, what happened in 2020?",
           hjust = 1, size = 3, family = "Roboto Condensed", fontface = "italic",
           color = "grey28")
p6
dev.off()


img7 <- image_graph(res = res, width = image_width, height = image_height)
p7 <- p6 + 
  annotation_custom(corona_grob, xmin = 2018.5, xmax = 2020, ymin = 61, ymax = 65)
p7
dev.off()


## Combine plots via morphing ----------

p_combined <- image_morph(c(img1, rep(img2, 2), img3, img4, img5, img6, rep(img7, 4)))
image_write_gif(p_combined, here(base_path, "plots", "govspend-de-morph.gif"))

  
  