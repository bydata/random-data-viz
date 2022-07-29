library(tidyverse)
library(ggstream)
library(ggtext)
library(lubridate)
library(here)
library(shadowtext)

base_path <- here("inflation-streamgraph")


# https://www.washingtonpost.com/business/2022/07/26/inflation-causes/
# https://www.bls.gov/news.release/cpi.t06.htm#cpipress6.f.1
#' Data source: Destatis
#' 1. Weights of products in the basket from 2015 onwards: 61111-0007_flat.csv
#'    https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0007&bypass=true&levelindex=1&levelid=1658874555194#abreadcrumb
#' 2. Consumer price index: 61111-0004_flat.csv
#'    https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0004&bypass=true&levelindex=0&levelid=1658875283615#abreadcrumb
#' Download data in flat format


## WEIGHTS ---------------------------------------------------------------------

weights <- read_csv2(here(base_path, "data", "61111-0007_flat.csv"),
                     locale = locale(encoding = "ISO-8859-15"))
# keep only the 2-digit codes
weights_level2 <- weights %>% 
  filter(str_detect(`2_Auspraegung_Code`, "^CC\\d{2}-\\d{2}$")) %>% 
  select(code = `2_Auspraegung_Code`, 
         label = `2_Auspraegung_Label`,
         weight = PRE999__Gewichtung_des_Verbraucherpreisindex__Promille)
weights_level2
sum(weights_level2$weight)


## CPI -------------------------------------------------------------------------

prices <- read_csv2(here(base_path, "data", "61111-0004_flat.csv"),
                    na = "...",
                     locale = locale(encoding = "ISO-8859-15"))

# keep only the 2-digit codes
prices_level2 <- prices %>% 
  filter(str_detect(`3_Auspraegung_Code`, "^CC\\d{2}-\\d{2}$")) %>% 
  select(code = `3_Auspraegung_Code`, 
         label = `3_Auspraegung_Label`,
         year = Zeit,
         month_code = `2_Auspraegung_Code`,
         index = `PREIS1__Verbraucherpreisindex__2015=100`) %>% 
  mutate(month = str_extract(month_code, "[01][0-9]$"),
         date = ymd(paste0(year, month, "01")),
         month = as.numeric(str_remove(month, "^0"))) %>% 
  arrange(code, date) %>% 
  group_by(code) %>% 
  mutate(prev_month_change = index / lag(index) - 1,
         prev_year_change = index / lag(index, 12) - 1) %>% 
  ungroup() %>% 
  select(-month_code) %>% 
  filter(year >= 2020 & !is.na(index)) 
head(prices_level2)

## DATA PREPARATION ------------------------------------------------------------

## join prices and weights
price_effects <- prices_level2 %>% 
  inner_join(weights_level2, by = c("code", "label")) %>% 
  # divide weights by 10 since it's per mill, not percent???
  mutate(effect_on_all_items = prev_month_change * weight / 1000,
         effect_on_all_items_yr = prev_year_change * weight / 1000)


# Check if the sum of effects corresponds with the overall inflation (compared to 
# previous month) in a particular month 
price_effects %>%
  filter(date == as_date("2022-06-01")) %>% 
  pull(effect_on_all_items_yr) %>% 
  sum()

price_effects %>% 
  filter(effect_on_all_items < 0) %>% 
  arrange(effect_on_all_items)


# Which product codes to choose or group?
price_effects %>% 
  group_by(code, label) %>% 
  summarize(across(prev_year_change, 
                   .fns = list("mean" = mean, "min" = min, "max" = max)), 
            .groups = "drop") %>% 
  arrange(-prev_year_change_max) 

## select product groups to be shown
labels_with_max_inflation <- c(
  "Verkehr", "Nahrungsmittel und alkoholfreie Getränke",
  "Bekleidung und Schuhe", "Wohnung, Wasser, Strom, Gas und andere Brennstoffe",
  "Freizeit, Unterhaltung und Kultur")


## PLOT ------------------------------------------------------------------------

#' Create a new transformation object to create a reverse date axis
#' https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot
#' https://groups.google.com/g/ggplot2/c/qrcvqy6TdzI

library(scales)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
  
}
rev_date <- c_trans("reverse", "date")

color_pal <- c("#99428A", "#86B26F", "#37649D", "#FBC754", "#AC3759", "#E2E2E2")
length(color_pal)


p <- price_effects %>% 
  mutate(label2 = ifelse(label %in% labels_with_max_inflation, label, "Andere"),
         label2 = factor(label2, levels = c(labels_with_max_inflation, "Andere")))  %>% 
  group_by(date, label2) %>% 
  summarize(effect_on_all_items = sum(effect_on_all_items_yr), .groups = "drop") %>% 
  ggplot(aes(date, effect_on_all_items, fill = label2)) +
  # light background for highlighting
  annotate("rect",
           xmin = as_date("2020-07-02"), xmax = as_date("2020-12-30"),
           ymin = -Inf, ymax = Inf,
           fill = "#EEF9FA"
  ) +
  geom_stream(extra_span = 0.01, bw = 0.8, n_grid = 1e4, type = "mirror", 
              sort = "none", true_range = "both") +
  scale_x_continuous(
    trans = rev_date, n.breaks = 6, position = "top",
    # format date labels to abbreviated month name + 2-digit year in all caps
    labels = function(x) {toupper(format(x, "%b %y"))}) +
  scale_fill_manual(values = color_pal) +
  coord_flip() +
  guides(fill = "none") +
  labs(
    title = "Inflation in Deutschland",
    caption = "Dargestellt ist die Veränderung des Verbraucherpreisindex (VPI) 
    zum Vorjahresmonat (COICOP 2-Steller Hierarchie,<br>
    mehrere Gruppen zu \"Andere\" zusammengefasst).
    Quelle: Statistisches Bundesamt, destatis.de. 
    Visualisierung: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.margin = margin(6, 6, 6, 6),
    legend.position = "bottom",
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey75", size = 0.2),
    panel.grid.minor.y = element_blank(),
    axis.text.y.right = element_text(
      color = rgb(158, 158, 158, maxColorValue = 255), margin = margin(l = 5)),
    plot.title = element_text(
      color = "black", family = "Libre Franklin SemiBold", size = 28, hjust = 0.5,
      margin = margin(t = 4, b = 8)),
    plot.caption = element_markdown(color = "grey30", lineheight = 1.1)
  )

## Text annotations
p_annotated <- p + 
  annotate("text",
           x = as_date("2020-08-15"), y = -0.05,
           label = "Befristete Senkung der Mehrwertsteuer\naufgrund der Coronavirus-Pandemie",
           hjust = 0, family = "Libre Franklin", size = 4
           ) +
  # Explanation of the stream width
  annotate("segment",
           x = as_date("2019-12-25"), xend = as_date("2019-12-25"),
           y = -0.008, yend = 0.008,
           col = "grey30", size = 0.25,
           arrow = arrow(type = "open", angle = 90, ends = "both", length = unit(1, "mm"))
  ) +
  annotate("text",
           x = as_date("2019-12-01"), y = 0,
           label = "So viel haben Produkte zur Entwicklung\nder Verbraucherpreise beigetragen",
           color = "grey30", 
           family = "Libre Franklin", size = 4, hjust = 0.5, vjust = 0.3) +
  # Highlight current inflation with text and a ruler
  annotate("segment",
           x = as_date("2022-06-15"), xend = as_date("2022-06-15"),
           y = -0.045, yend = 0.045,
           col = "grey30", size = 0.25,
           arrow = arrow(type = "open", angle = 90, ends = "both", length = unit(1, "mm"))
           ) +
  annotate("text",
           x = as_date("2022-07-01"), y = 0,
           label = "Inflation Juni 2022: 7,6 %", color = "grey30", 
           family = "Libre Franklin", size = 4, hjust = 0.5) +
  # Products: Wohnung
  annotate(GeomShadowText,
           x = as_date("2022-05-01"), y = -0.0075,
           label = "Wohnung, Wasser,\nStrom, Gas", color = "grey9", 
           family = "Libre Franklin", size = 3.5, bg.color = "white", hjust = 0.5) +
  annotate("text",
           x = as_date("2022-05-01"), y = 0.03,
           label = "Verkehr", color = "grey99", 
           family = "Libre Franklin", size = 3.5, hjust = 0.5) +
  # Products: Nahrungsmittel
  annotate("text",
           x = as_date("2021-12-01"), y = 0.03,
           label = "Nahrungsmittel &\nalkoholfreie Getränke", color = "grey9", 
           family = "Libre Franklin", size = 3.5, hjust = 0) +
  annotate("segment",
           x = as_date("2021-12-01"), xend = as_date("2021-12-01"), 
           y = 0.0295, yend = 0.0075, color = "grey9", size = 0.2) +
  # Products: Bekleidung & Schuhe
  annotate("text",
           x = as_date("2021-08-01"), y = 0.03,
           label = "Bekleidung &\nSchuhe", color = "grey9", 
           family = "Libre Franklin", size = 3.5, hjust = 0) +
  annotate("segment",
           x = as_date("2021-08-01"), xend = as_date("2021-08-01"), 
           y = 0.0295, yend = 0.0015, color = "grey9", size = 0.2) +
  # Products: Freizeit
  annotate("text",
           x = as_date("2021-10-01"), y = -0.05,
           label = "Freizeit, Unterhaltung,\nKultur", color = "grey9", 
           family = "Libre Franklin", size = 3.5, hjust = 0) +
  annotate("segment",
           x = as_date("2021-10-01"), xend = as_date("2021-10-01"), 
           y = -0.028, yend = -0.0125, color = "grey9", size = 0.2) +
  # Products: Andere
  annotate("text",
           x = as_date("2022-01-01"), y = -0.05,
           label = "Andere Waren &\nDienstleistungen", color = "grey9", 
           family = "Libre Franklin", size = 3.5, hjust = 0) +
  annotate("segment",
           x = as_date("2022-01-01"), xend = as_date("2022-01-01"), 
           y = -0.032, yend = -0.022, color = "grey9", size = 0.2)

ggsave(here(base_path, "plots", "streamgraph-inflation-de.png"),
       dpi = 600, width = 8, height = 10)

