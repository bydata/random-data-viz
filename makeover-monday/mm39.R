library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(ggdist)
library(here)

base_path <- here("makeover-monday")

data_url <- "https://raw.githubusercontent.com/EDJNet/internet_speed/main/data/nuts_3/nuts_3_2022_2.csv"
# data_url <- "https://raw.githubusercontent.com/EDJNet/internet_speed/main/data/nuts_2/nuts_2_2022_2.csv"
df <- read_csv(data_url)
glimpse(df)

df %>%
  arrange(-avg_d)
df %>%
  arrange(-avg_u)

df <- df %>%
  mutate(country_code = str_sub(id, 1, 2),
         country_name = countrycode::countrycode(country_code, origin = "iso2c", 
                                                 destination = "country.name"),
         country_name = case_when(
           country_code == "EL" ~ "Greece",
           country_code == "XK" ~ "Kosovo",
           country_code == "UK" ~ "United Kingdom",
           country_code == "BI" ~ "Bosnia-Herzegovina",
           TRUE ~ country_name
         ))


#' Inspiration: 
#' http://www.maartenlambrechts.com/2019/09/04/splitting-EU-regions-making-of.html

df_plot <- df  %>% 
  na.omit() %>%
  mutate(avg_d_dev = avg_d - mean(avg_d)) %>% 
  group_by(country_name) %>% 
  mutate(avg_d_country = mean(avg_d)) %>% 
  ungroup() %>% 
  mutate(country_name = fct_reorder(country_name, avg_d_country))

capitals <- c(
  "AL" = "Tiranë",
  "AT"= "Wien",
  "BE" = "Arr. de Bruxelles-Capitale/Arr. Brussel-Hoofdstad",
  "BG" = "Sofia (stolitsa)",
  "BI" = "Sarajevo",
  "CH" = "Bern",
  "CY" = "Kýpros",
  "CZ" = "Hlavní město Praha",
  "DE" = "Berlin",
  "DK" = "Byen København",
  "EE" = "Põhja-Eesti",
  "EL" = "Kentrikos Tomeas Athinon",
  "ES" = "Madrid",
  "HR" = "Grad Zagreb",
  "LI" = "Liechtenstein",
  "FI" = "Helsinki-Uusimaa",
  "FR" = "Paris",
  "HU" = "Budapest",
  "IE" = "Dublin",
  "IS" = "Höfuðborgarsvæði",
  "IT" = "Roma",
  "LT" = "Vilniaus apskritis",
  "LU" = "Luxembourg",
  "LV" = "Rīga",
  "MD" = "Chişinău",
  "ME" = "Crna Gora",
  "MK" = "Skopski",
  "MT" = "Malta",
  "NL" = "Groot-Amsterdam",
  "NO" = "Oslo",
  "PL" = "Miasto Warszawa",
  "PT" = "Área Metropolitana de Lisboa",
  "RO" = "Bucureşti",
  "RS" = "City of Belgrade",
  "SE" = "Stockholms län",
  "SI" = "Osrednjeslovenska",
  "SK" = "Bratislavský kraj",
  "TR" = "Ankara",
  "UA" = "Київська",
  "UK" = "Camden and City of London",
  "XK" = "Pristina"
)

df_plot %>% 
  ggplot(aes(as.numeric(country_name), avg_d)) +
  geom_hline(aes(yintercept = mean(avg_d)), color = "grey20", size = 0.3) +
  annotate("label", label = "Average",
           x = 0.5, y = mean(df_plot$avg_d), hjust = 0, label.size = 0, 
           fill = alpha("grey20", 0.2), family = "Roboto Condensed", size = 3)+ 
  geom_point(aes(fill = avg_d_dev),
             alpha = 0.8, shape = 21, color = "grey90", stroke = 0.1, size = 2) +
  geom_point(
    data = subset(df_plot, name %in% capitals),
    color = "grey8", shape = 21, fill = NA, size = 2
  ) +
  scale_x_continuous(
    breaks = seq_along(levels(df_plot$country_name)),
    labels = levels(df_plot$country_name),
    sec.axis = sec_axis(
      trans = ~., breaks = seq_along(levels(df_plot$country_name)),
      labels = levels(df_plot$country_name)
      ),
    expand = expansion(add = c(0.5, 0.25))) +
  scale_y_continuous(breaks = seq(0, 300, 50), position = "right") +
  colorspace::scale_fill_binned_diverging(
    palette = "Purple-Green", n_interp = 5) +
  guides(fill = guide_colorbar(title.position = "top")) +
  coord_flip() +
  labs(
    title = "Quality of Internet Speed in European Regions",
    subtitle = "Average download speed in European regions at regional level (NUTS-3)
    based on Speedtest measurements on fixed and mobile (2022 Q2).<br><br>
    \U25EF marks the capital or the region containing the capital city
    ",
    caption = "Source: European Data Journalism Network, 
    Ookla Global Fixed and Mobile Network Performance Maps.<br>
    Visualisation: Ansgar Wolsing (Inspiration: Maarten Lambrechts,
    *'Why Budapest, Warsaw and Lithuania split themselves in two'*)",
    x = NULL,
    y = "Avg. download speed (mbit/s)",
    fill = "Deviation from average of all regions"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = c(0.125, 1.14),
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(15, "mm"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "grey70"),
    text = element_text(color = "grey24"),
    axis.text = element_text(color = "grey24"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.98, hjust = 0, lineheight = 1.1,
      # make space for the legend
      margin = margin(t = 4, b = 60)),
    plot.caption = element_textbox(width = 0.98, hjust = 0, lineheight = 1.1),
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "makeover-monday-2022-39-europe.png"), width = 6, 
       height = 8.5, scale = 1)
