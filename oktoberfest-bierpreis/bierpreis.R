library(tidyverse)
library(here)
library(ggtext)

base_path <- here("oktoberfest-bierpreis")


#' Bierpreise Oktoberfest:
#' https://opendata.muenchen.de/de/dataset/oktoberfest/resource/52fda541-c4dd-4eb6-8f2a-84ca24ffe775
#' Vor 2002 Preise umgerechnet in EUR

bierpreis_url <- "https://opendata.muenchen.de/dataset/8d6c8251-7956-4f92-8c96-f79106aab828/resource/52fda541-c4dd-4eb6-8f2a-84ca24ffe775/download/oktoberfestbierpreis19852019.csv"
bier <- read_csv(bierpreis_url)


#' Mittlerer Bierpreis 2022
#' Quelle: https://www.oktoberfest.de/informationen/service-fuer-besucher/der-bierpreis
# Die Bierpreise 2022 der großen Zelte im Detail:
# Armbrustschützen-Festzelt: 13,50 Euro (2019: 11,70 Euro)
# Augustiner-Festhalle: 12,80 Euro (2019: 11,40 Euro)
# Bräurosl: 13,40 Euro (2019: 11,80 Euro)
# Fischer-Vroni: 12,90 Euro (2019: 11,35 Euro)
# Hacker-Festzelt: 13,40 Euro (2019: 11,70 Euro)
# Hofbräuhaus-Festzelt: 13,60 Euro (2019: 11,40 Euro)
# Käfer Wiesn-Schänke: 13,70 Euro (2019: 11,80 Euro)
# Löwenbräu-Festzelt: 13,60 Euro (2019: 11,80 Euro)
# Marstall: 13,70 Euro (2019: 11,80 Euro)
# Ochsenbraterei: 13,50 Euro (2019: 11,50 Euro)
# Paulaner-Festzelt: 13,50 Euro (2019: 11,80 Euro)
# Schottenhamel-Festhalle: 13,60 Euro (2019: 11,45 Euro)
# Schützen-Festzelt: 13,70 Euro (2019: 11,80 Euro)
# Weinzelt (Weißbier): 16,80 Euro (2019: 15,90 Euro)
# Oide Wiesn:
# Festzelt Tradition: 13,20 Euro (2019: 11,40 Euro)
# Herzkasperl Festzelt: 13,40 Euro (2019: 11,40 Euro)
# Museumszelt: 12,60 Euro (2019: 10,80 Euro)
# Volkssängerzelt: 12,90 Euro (2019: 11,40 Euro)

(bierpreis_2022 <- round(mean(c(13.5, 12.8, 13.4, 12.9, 13.4, 13.6, 13.7, 13.6, 13.7, 
                         13.5, 13.5, 13.6, 13.7, 16.8,
                         13.2, 13.4, 12.6, 12.9)), 2))

bier <- bier %>% 
  add_row(jahr = 2020:2021, bier_preis = NA) %>% 
  add_row(jahr = 2022, bier_preis = bierpreis_2022) %>% 
  mutate(index_bier = 100 * bier_preis / .$bier_preis[which(.$jahr == 1991)])


bier %>% 
  ggplot(aes(jahr, bier_preis)) +
  geom_step() +
  geom_point(data = . %>% filter(jahr == max(jahr))) +
  ggrepel::geom_text_repel(data = . %>% filter(jahr == max(jahr)),
            aes(label = jahr)) +
  theme_minimal()


#' Verbraucherpreisindex DE
#' https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0002&bypass=true&levelindex=0&levelid=1663532389986#abreadcrumb
cpi <- read_csv2(here(base_path, "61111-0002_flat.csv"), locale = locale(encoding = "ISO-8859-15"))
cpi <- cpi %>% 
  mutate(monat = str_remove(`2_Auspraegung_Code`, "MONAT0?"),
         monat = as.numeric(monat)) %>% 
  filter(monat == 8) %>% 
  select(jahr = Zeit, monat, index_2015 = `PREIS1__Verbraucherpreisindex__2015=100`) %>% 
  mutate(index_2015 = as.numeric(str_replace(index_2015, ",", "."))) %>% 
  mutate(index_1991 = 100 * index_2015 / .$index_2015[which(.$jahr == 1991 & .$monat == 8)]) %>% 
  select(-monat)
cpi

#' Verbraucherpreise Bier (CC13-0213) und Speisen und Getränke in Restaurant, Cafe, Bar u.Ä. (CC13-1111(1))
#' https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0004&bypass=true&levelindex=0&levelid=1663532389986#abreadcrumb
cpi_bier_gastro <- read_csv2(here(base_path, "61111-0004_flat_CC13-0213-CC13-1111.csv"), locale = locale(encoding = "ISO-8859-15"))
cpi_bier_gastro <- cpi_bier_gastro %>% 
  mutate(monat = str_remove(`2_Auspraegung_Code`, "MONAT0?"),
         monat = as.numeric(monat)) %>% 
  filter(monat == 8) %>% 
  select(jahr = Zeit, monat, code = `3_Auspraegung_Code`, 
         index_2015 = `PREIS1__Verbraucherpreisindex__2015=100`) %>% 
  filter(code != "CC13-11111") %>% 
  group_by(code) %>% 
  arrange(jahr, .by_group = TRUE) %>% 
  mutate(index_2015 = as.numeric(str_replace(index_2015, ",", "."))) %>% 
  mutate(index_1991 = 100 * index_2015 / .$index_2015[which(.$code == code & .$jahr == 1991 & .$monat == 8)]) %>%
  ungroup() %>% 
  select(-monat)
cpi_bier_gastro_wide <- cpi_bier_gastro %>% 
  pivot_wider(id_cols = jahr, names_from = "code", values_from = c("index_2015", "index_1991"),
              names_repair = janitor::make_clean_names)


# bier %>% 
#   filter(jahr >= 1991) %>% 
#   inner_join(cpi, by = "jahr") %>% 
#   inner_join(cpi_bier_gastro_wide, by = "jahr") %>% 
#   ggplot(aes(jahr)) +
#   geom_step(aes(y = index_bier, color = "Wiesn")) +
#   geom_step(
#     data = ~subset(., !is.na(bier_preis)),
#     aes(y = index_bier, color = "Wiesn"),
#     linetype = "dashed") +
#   geom_step(aes(y = index_1991, color = "Verbraucherpreisindex (August)")) +
#   geom_step(aes(y = index_bier_1991, color = "Verbraucherpreisindex BIER (August)")) +
#   geom_point(
#     aes(y = index_bier, color = "Wiesn"),
#     data = . %>% filter(jahr == max(jahr))) +
#   ggrepel::geom_text_repel(
#     data = . %>% filter(jahr == max(jahr)),
#     aes(y = index_bier, label = jahr)) +
#   theme_minimal() +
#   theme(
#     plot.background = element_rect(color = "white", fill = "white"),
#     legend.position = "top"
#   )
# ggsave(here(base_path, "bierpreise.png"), width = 6, height = 5)



line_colors <- c("grey70", "grey40", "grey20", "#0393CD")

bier %>% 
  filter(jahr >= 1991) %>% 
  inner_join(cpi, by = "jahr") %>% 
  inner_join(cpi_bier_gastro_wide, by = "jahr") %>% 
  select(jahr, index_bier, matches("index.*_1991")) %>% 
  pivot_longer(cols = -jahr, names_to = "index_type", values_to = "value") %>% 
  ggplot(aes(jahr, value, color = index_type)) +
  geom_step(size = 0.8) +
  geom_step(
    data = ~subset(., !is.na(value)),
    linetype = "dotted", size = 0.8) +
  geom_point(
    data = . %>% filter(jahr == max(jahr))) +
  # ggrepel::geom_text_repel(
  #   data = . %>% filter(jahr == max(jahr), index_type == "index_bier"),
  #   aes(label = jahr),
  #   family = "Roboto Condensed", direction = "x", hjust = 0) +
  geom_text(
    data = . %>% filter(jahr == max(jahr), index_type == "index_bier"),
    aes(label = jahr),
    family = "Roboto Condensed", hjust = -0.2) +
  annotate(
    "richtext",
    label = c("Warenkorb<br>gesamt", 
              "Bier", "Gastronomie", "**Mittlerer Bierpreis Wiesn**"),
    # x = c(2020, 2016, 2021),
    x = c(rep(2022.1, 3), 2016),
    y = c(180, 160, 199, 260),
    color = line_colors,
    size = c(3, 3, 3, 4),
    hjust = c(0, 0, 0, 1), family = "Roboto Condensed", label.size = 0, fill = NA,
    lineheight = 0.8
  ) +
  scale_x_continuous(limits = c(NA, 2025)) +
  scale_y_continuous() +
  scale_color_manual(values = line_colors) +
  coord_cartesian(ylim = c(NA, 300), clip = "off") +
  guides(color = "none") +
  labs(
    title = "Preis für <span style='color:#0393CD'>Maß Bier</span> auf der Wiesn
    mehr als verdreifacht seit 1991",
    subtitle = "Ein Indexwert von 100 entspricht den Preisen von 1991",
    caption = "Für die Verbraucherpreise wurden die monatlichen Werte für August verwendet.<br>
    Gastronomie = CC13-1111: Restaurants, Cafes, Straßenverkauf und Ähnliches.<br>
    Daten Oktoberfest: Stadt München (1991-2019), oktoberfest.de (2022),
    Verbraucherpreise: Statistisches Bundesamt.<br>Visualisierung: Ansgar Wolsing",
    x = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(color = "grey24"),
    plot.title = element_markdown(color = "black", face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_markdown(lineheight = 1.1, hjust = 0)
  )
ggsave(here(base_path, "bierpreise.png"), width = 6.5, height = 5)


