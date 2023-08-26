library(tidyverse)
library(ggtext)
library(here)

base_path <- here("waschmaschinen")

df <- read_csv2(here(base_path, "51000-0012_flat.csv"),
                locale = locale(encoding = "ISO-8859-15"),
                na = c("..."))


# Zeichen	Bedeutung
# 0	weniger als die Hälfte von 1 in der letzten besetzten Stelle, jedoch mehr als nichts
# -	nichts vorhanden
# ...	Angabe fällt später an
# /	keine Angaben, da Zahlenwert nicht sicher genug
# .	Zahlenwert unbekannt oder geheimzuhalten


df_prep <- df %>% 
  select(jahr = Zeit, 
         monat_code = `3_Auspraegung_Code`, 
         monat = `3_Auspraegung_Label`,
         zielland = `2_Auspraegung_Label`, 
         `4_Auspraegung_Label`,
         wert_eur = `WERTA__Ausfuhr:_Wert__Tsd._EUR`) %>% 
  mutate(zielland2 = str_remove(zielland, " \\(.+\\)"),
         monat_num = str_remove(monat_code, "MONAT0?") %>% as.numeric(),
         wert_eur = ifelse(wert_eur == "-", 0, as.numeric(wert_eur)))

gus_countries <- c(
  "Armenien", "Aserbaidschan", "Belarus", "Kasachstan", "Kirgisistan",
  "Republik Moldau", "Russische Föderation", "Tadschikistan", "Turkmenistan", "Usbekistan"
)

# Check if all countries are selected
df_prep %>% 
  filter(zielland2 %in% gus_countries) %>% 
  distinct(zielland2) %>% 
  arrange(zielland2)

df_prep %>% 
  filter(zielland2 %in% gus_countries, zielland2 != "Russische Föderation") %>% 
  filter(monat_num <= 6) %>% 
  group_by(zielland2, jahr) %>% 
  summarize(total_wert_eur = sum(wert_eur), .groups = "drop") %>%
  ggplot(aes(jahr, total_wert_eur)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(zielland2)) +
  theme_light()


df_prep %>% 
  filter(zielland2 == "Kasachstan") %>% 
  filter(monat_num <= 6) %>% 
  group_by(zielland2, jahr) %>% 
  summarize(total_wert_eur = sum(wert_eur), .groups = "drop") %>% 
  ggplot(aes(jahr, total_wert_eur)) +
  geom_area(col = "steelblue", fill = alpha("steelblue", 0.2), linewidth = 0.8) +
  geom_point(shape = 21, fill = "white", col = "steelblue", stroke = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Haha, Waschmaschinen",
    subtitle = "Export von Maschinen zum Waschen oder Trocknen von Wäsche aus
    Deutschland nach **Kasachstan** im 1. Halbjahr",
    caption = "Daten: Statistisches Bundesamt (51000-0012). Visualisierung: Ansgar Wolsing",
    x = NULL,
    y = "Wert in Tausend EUR"
  ) + 
  theme_light(base_family = "Roboto") +
  theme(
    text = element_text(color = "grey30"),
    plot.title = element_markdown(face = "bold", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.9, lineheight = 1.2),
    plot.caption = element_markdown(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(color = "grey10", size = 0.33)
  )
ggsave(here(base_path, "waschmaschinen-kasachstan.png"), width = 6, height = 4)
