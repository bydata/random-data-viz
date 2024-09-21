library(tidyverse)
library(here)
library(ggtext)

base_path <- here("oktoberfest-bierpreis")


#' Bierpreise Oktoberfest:
#' https://opendata.muenchen.de/de/dataset/oktoberfest/resource/52fda541-c4dd-4eb6-8f2a-84ca24ffe775
#' Vor 2002 Preise umgerechnet in EUR

bierpreis_url <- "https://opendata.muenchen.de/dataset/8d6c8251-7956-4f92-8c96-f79106aab828/resource/52fda541-c4dd-4eb6-8f2a-84ca24ffe775/download/oktoberfestbierpreis19852019.csv"
bier <- read_csv(bierpreis_url)


#' Mittlerer Bierpreis 2023
#' Quelle: https://www.oktoberfest.de/informationen/service-fuer-besucher/der-bierpreis
# Die Bierpreise 2024 der großen Zelte im Detail:
# Armbrustschützen-Festzelt: 14,95 Euro (2023: 14,40 Euro)
# Augustiner-Festhalle: 14,10 Euro (2023: 13,50 Euro)
# Bräurosl: 15,10 Euro (2023: 14,30 Euro)
# Fischer-Vroni: 14,70 Euro (2023: 13,70 Euro)
# Hacker-Festzelt: 15,10 Euro (2023: 14,40 Euro)
# Hofbräuhaus-Festzelt: 14,95 Euro (2023: 14,50 Euro)
# Käfer Wiesn-Schänke: 14,90 Euro (2023: 14,50 Euro)
# Löwenbräu-Festzelt: 15 Euro (2023: 14,50 Euro)
# Marstall: 15 Euro (2023: 14,50 Euro)
# Ochsenbraterei: 14,90 Euro (2023: 14,50 Euro)
# Paulaner-Festzelt: 15,10 Euro (2023: 14,50 Euro)
# Schottenhamel-Festhalle: 14,95 Euro (2023: 14,50 Euro)
# Schützen-Festzelt: 14,90 Euro (2023: 14,50 Euro)
# Kufflers Weinzelt (Weißbier): 17,40 Euro (2023: 17,40 Euro)


bierpreise_aktuell <- c(14.95, 14.1, 15.1, 14.7, 15.1, 14.95, 14.9, 15.0, 14.9, 
                     15.0, 15.0, 14.9, 15.1, 14.95, 14.9, 17.4)
(bierpreis_mean_aktuell <- round(mean(bierpreise_aktuell), 2))
(current_year <- year(Sys.Date()))

bier <- bier %>% 
  # No Wiesn due Coronavirus pandemic
  add_row(jahr = 2020:2021, bier_preis = NA) %>% 
  add_row(jahr = current_year, bier_preis = bierpreis_mean_aktuell) %>% 
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
cpi <- read_csv2(here(base_path, "61111-0002_$F_flat.csv"), locale = locale(encoding = "ISO-8859-15"))
cpi <- cpi %>% 
  mutate(monat = str_remove(`2_Auspraegung_Code`, "MONAT0?"),
         monat = as.numeric(monat)) %>% 
  filter(monat == 8) %>% 
  select(jahr = Zeit, monat, index_2020 = `PREIS1__Verbraucherpreisindex__2020=100`) %>% 
  mutate(index_2020 = as.numeric(str_replace(index_2020, ",", "."))) %>% 
  mutate(index_1991 = 100 * index_2020 / .$index_2020[which(.$jahr == 1991 & .$monat == 8)]) %>% 
  select(-monat)
cpi

#' Verbraucherpreise Bier (CC13-0213) und Speisen und Getränke in Restaurant, Cafe, Bar u.Ä. (CC13-1111(1))
#' https://www-genesis.destatis.de/genesis//online?operation=table&code=61111-0004&bypass=true&levelindex=0&levelid=1663532389986#abreadcrumb
cpi_bier_gastro <- read_csv2(here(base_path, "61111-0004_$F_flat.csv"), locale = locale(encoding = "ISO-8859-15"))
cpi_bier_gastro <- cpi_bier_gastro %>% 
  mutate(monat = str_remove(`2_Auspraegung_Code`, "MONAT0?"),
         monat = as.numeric(monat)) %>% 
  filter(monat == 8) %>% 
  select(jahr = Zeit, monat, code = `3_Auspraegung_Code`, 
         index_2020 = `PREIS1__Verbraucherpreisindex__2020=100`) %>% 
  filter(code %in% c("CC13-0213", "CC13-1111")) %>% 
  group_by(code) %>% 
  arrange(jahr, .by_group = TRUE) %>% 
  mutate(index_2020 = as.numeric(str_replace(index_2020, ",", "."))) %>% 
  mutate(index_1991 = 100 * index_2020 / .$index_2020[which(.$code == code & .$jahr == 1991 & .$monat == 8)]) %>%
  ungroup() %>% 
  select(-monat)

cpi_bier_gastro_wide <- cpi_bier_gastro %>% 
  pivot_wider(id_cols = jahr, names_from = "code", values_from = c("index_2020", "index_1991"),
              names_repair = janitor::make_clean_names)


line_colors <- c("grey70", "grey40", "grey20", "#0393CD")

df_plot <- bier %>% 
  filter(jahr >= 1991) %>% 
  inner_join(cpi, by = "jahr") %>% 
  inner_join(cpi_bier_gastro_wide, by = "jahr") %>% 
  select(jahr, index_bier, matches("index.*_1991")) %>% 
  pivot_longer(cols = -jahr, names_to = "index_type", values_to = "value") |> 
  arrange(jahr)

df_plot %>% 
  ggplot(aes(jahr, value, color = index_type)) +
  geom_step(linewidth = 0.8) +
  geom_step(
    data = ~subset(., !is.na(value)),
    linetype = "dotted", linewidth = 0.8) +
  geom_point(
    data = . %>% filter(jahr == max(jahr))) +
  geom_text(
    data = . %>% filter(jahr == max(jahr), index_type == "index_bier"),
    aes(label = jahr),
    family = "Roboto Condensed", hjust = -0.2) +
  annotate(
    "richtext",
    label = c("Warenkorb<br>gesamt", 
              "Bier", "Gastronomie", "**Mittlerer Bierpreis Wiesn**"),
    x = c(rep(2024, 3), 2016),
    y = c(193, 180, 230, 260),
    color = line_colors,
    size = c(3, 3, 3, 4),
    hjust = c(0, 0, 0, 1), family = "Roboto Condensed", label.size = 0, fill = NA,
    lineheight = 0.8
  ) +
  annotate(
    "text",
    label = str_wrap(c("1991 kostete die Maß umgerechnet 4,21 EUR",
                       sprintf("%d liegt der Preis im Mittel bei %0.2f EUR", 
                               current_year, bierpreis_mean_aktuell)), 24),
    x = c(1990, 2017), 
    y = c(135, 350),
    hjust = 0, size = 2.5, family = "Roboto Condensed", color = "grey30"
  ) + 
  scale_x_continuous(
    breaks = seq(1990, 2024, 5),
    limits = c(NA, current_year + 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = line_colors) +
  coord_cartesian(ylim = c(NA, 350), clip = "off") +
  guides(color = "none") +
  labs(
    title = "Preis für die <span style='color:#0393CD'>Maß Bier</span> auf der Wiesn
     seit 1991 mehr als verdreifacht",
    subtitle = "Ein Indexwert von 100 entspricht den Preisen von 1991",
    caption = "Für die Verbraucherpreise wurden die monatlichen Werte für August 
    verwendet. 
    Gastronomie = CC13-1111: Restaurants, Cafes, Straßenverkauf und Ähnliches. 
    Daten Oktoberfest: Stadt München (1991-2023), oktoberfest.de, 
    ungewichtetes Mittel (2024),
    Verbraucherpreise: Statistisches Bundesamt. 
    Visualisierung: Ansgar Wolsing",
    x = NULL, y = "Index (Preise 1991 = 100)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(color = "grey30"),
    plot.title = element_markdown(color = "black", face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.1, hjust = 0, width = 1,
      margin = margin(t = 6, b = 6)),
    axis.ticks.x = element_line(linewidth = 0.2)
  )
ggsave(here(base_path, "bierpreise-2024.png"), width = 6.5, height = 5.5)
