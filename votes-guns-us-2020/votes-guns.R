library(tidyverse)
library(ggtext)
library(here)
library(glue)

base_path <- "votes-guns-us-2020"

# Source: https://en.wikipedia.org/wiki/2020_United_States_presidential_election#Results
election <- read_tsv(here(base_path, "election-results.tsv"))
percent_rep_overall <- 46.86
# Source: https://www.cdc.gov/nchs/pressroom/sosmap/firearm_mortality/firearm.htm
gun_deaths <- read_csv(here(base_path, "gun-deaths-us-2020.csv"), name_repair = tolower)

election %>% 
  anti_join(gun_deaths, by = "state")
gun_deaths %>% 
  anti_join(election, by = "state")
gun_deaths %>% distinct(state) %>% View()
election %>% arrange(state) %>% distinct(state) %>% View()

election %>% 
  anti_join(gun_deaths, by = "state")

df <- gun_deaths %>% 
  filter(year == 2020) %>% 
  inner_join(election, by = "state") %>% 
  mutate(across(c(percent_dem, percent_rep), ~as.numeric(str_remove(.x, "%"))))

mean_gun_death_rate <- mean(gun_deaths$rate)

df %>% 
  ggplot(aes(percent_rep, rate)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = state),
                           size = 2, color = "grey40") +
  geom_smooth(method = "lm") +
  theme_bw()

# Assault Weapon Bans
# Source: https://giffords.org/lawcenter/gun-laws/policy-areas/hardware-ammunition/assault-weapons/
states_assault_weapon_ban <- c("CA", "CT", "DC", "MD", "HI", "MA", "NJ", "NY")



df %>% 
  mutate(assault_weapon_ban = ifelse(state %in% states_assault_weapon_ban, "yes", "no")) %>% 
  ggplot(aes(percent_rep, rate)) +
  annotate("rect", 
           xmin = 50, 
           xmax = Inf, 
           ymin = mean_gun_death_rate, 
           ymax = Inf,
           fill = "grey72", alpha = 0.3) +
  geom_hline(yintercept = mean_gun_death_rate, col = "grey30", size = 0.3) +
  geom_vline(xintercept = 50, col = "grey30", size = 0.3) +
  # annotate("richtext", x = 72, y = 31, hjust = 1,
  #          label = "High GOP, high gun death rate",
  #          size = 3, fill = NA, label.size = 0, family = "Noto Sans") +
  annotate("richtext", 
           x = c(72, 29, 29, 72), 
           y = c(31, 31, 0, 0), 
           hjust = c(1, 0, 0, 1),
           label = c("High GOP, high gun death rate", 
                     "Low GOP, high gun death rate", 
                     "Low GOP, low gun death rate", 
                     "High GOP, low gun death rate"),
           color = "grey30", size = 3, fill = NA, label.size = 0, family = "Noto Sans") +

      # geom_point(aes(fill = ifelse(percent_rep > percent_dem, "Republicans", "Democrats")),
      #        size = 2.5, shape = 21, color = "white", stroke = 0.2) +
    geom_point(aes(fill = ifelse(percent_rep > percent_dem, "Republicans", "Democrats"),
                   shape = assault_weapon_ban),
               size = 2.5, color = "white", stroke = 0.2) +
    
  ggrepel::geom_text_repel(
    aes(label = state),
    point.padding = 1e-07, size = 2, color = "grey40") +
  scale_fill_manual(values = c("darkblue", "darkred")) +
    scale_shape_manual(values = c(21, 22)) +
  coord_cartesian(clip = "off") +
  guides(
    fill = guide_legend(override.aes = list(shape = 21), title.position = "top"),
    shape = guide_legend(override.aes = list(color = "grey30"), title.position = "top")
  ) +
  labs(
    title = "Votes for Trump in the 2020 U.S. Presidential Elections vs.<br>
    gun deaths per 100,000 inhabitants in 2020",
    caption = "Sources: CDC, Giffords Law Center, Wikipedia",
    x = "Votes for Trump 2020 (%)",
    y = "Gun deaths per 100,000",
    fill = "Dominant party",
    shape = "Assault weapon ban"
  ) +
  theme_bw(base_family = "Noto Sans") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.spacing = unit(1, "mm"),
    legend.spacing.y = unit(1, "mm"), 
    text = element_text(color = "grey30"),
    plot.title = element_markdown(lineheight = 1.1, color = "black"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "votes-guns-us.png"), width = 6, height = 5)

with(df, cor(rate, percent_rep))
