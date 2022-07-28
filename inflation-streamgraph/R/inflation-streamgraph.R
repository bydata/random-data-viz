library(tidyverse)
library(ggstream)
library(ggtext)
library(lubridate)
library(here)

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
         # month = as.numeric(month),
         date = ymd(paste0(year, month, "01")),
         month = as.numeric(str_remove(month, "^0"))) %>% 
  arrange(code, date) %>% 
  group_by(code) %>% 
  mutate(prev_month_change = index/lag(index) - 1) %>% 
  ungroup() %>% 
  select(-month_code) %>% 
  filter(year >= 2021 & !is.na(index)) 
  # filter(!is.na(index))
head(prices_level2)


## join prices and weights
price_effects <- prices_level2 %>% 
  inner_join(weights_level2, by = c("code", "label")) %>% 
  # divide weights by 10 since it's per mill, not percent???
  mutate(effect_on_all_items = prev_month_change * weight / 1000)

# Check if the sum of effects corresponds with the overall inflation (compared to 
# previous month) in a particular month 
price_effects %>%
  filter(date == as_date("2021-03-01")) %>% 
  pull(effect_on_all_items) %>% 
  sum()

price_effects %>% 
  filter(effect_on_all_items < 0) %>% 
  arrange(effect_on_all_items)


## PLOT -------------------------------------


# https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot

library(scales)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
  
}
rev_date <- c_trans("reverse", "time")



price_effects %>% 
  mutate(date = as.POSIXct(date)) %>% 
  ggplot(aes(date, effect_on_all_items, fill = label)) +
  geom_stream(extra_span = 0.06, bw = 0.8, n_grid = 1e4) +
  scale_x_continuous(trans = rev_date) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# Which product codes to choose or group?
price_effects %>% 
  group_by(code, label) %>% 
  summarize(across(prev_month_change, .fns = list("min" = min, "max" = max, "range" = range)), .groups = "drop") %>% 
  arrange(prev_month_change_range)

labels_with_max_inflation <- price_effects %>% 
  group_by(code, label) %>% 
  summarize(prev_month_change_max = max(prev_month_change), .groups = "drop") %>% 
  arrange(-prev_month_change_max) %>% 
  slice_max(order_by = prev_month_change_max, n = 6) %>% 
  pull(label)
labels_with_max_inflation

color_pal <- c("#784377", "#96B07C", "#486193", "#ECCA77", "#934658", "#C0C0C0", "#E2E2E2")
length(color_pal)



p <- price_effects %>% 
  mutate(date = as.POSIXct(date),
         label2 = ifelse(label %in% labels_with_max_inflation, label, "other"),
         label2 = factor(label2, levels = c(labels_with_max_inflation, "other")))  %>% 
  group_by(date, label2) %>% 
  summarize(effect_on_all_items = sum(effect_on_all_items), .groups = "drop") %>% 
  ggplot(aes(date, effect_on_all_items, fill = label2)) +
  geom_stream(extra_span = 0.06, bw = 0.8, n_grid = 1e4, type = "mirror", 
              sort = "none", true_range = "both") +
  scale_x_continuous(
    trans = rev_date, n.breaks = 6, position = "top",
    labels = function(x) {toupper(format(x, "%b %y"))}) +
  scale_fill_manual(values = color_pal) +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1)) +
  theme_void(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.margin = margin(6, 6, 6, 6),
    legend.position = "bottom",
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey75", size = 0.2),
    axis.text.y.right = element_text(color = rgb(158, 158, 158, maxColorValue = 255))
  )
ggsave(here(base_path, "plots", "streamgraph-inflation-de.png"),
       dpi = 600, width = 8, height = 10)





# ---------


# Area chart
price_effects %>% 
  mutate(date = as.POSIXct(date)) %>% 
  ggplot(aes(date, effect_on_all_items, fill = label)) +
  geom_area() +
  scale_x_continuous(trans = rev_date) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
     

# Stacked bar chart
price_effects %>% 
  mutate(date = as.POSIXct(date)) %>% 
  ggplot(aes(date, effect_on_all_items, fill = label)) +
  geom_col(position = "stack") +
  scale_x_continuous(trans = rev_date) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
