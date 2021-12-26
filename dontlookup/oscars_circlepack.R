pacman::p_load("tidyverse", "packcircles", "ggplot2", "viridis", "here", "glue",
               "ggtext", "patchwork")

# Create data
data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 

oscars <- read_csv(here("dontlookup", "oscars.csv"))
oscars_long <- oscars %>% 
  select(-imdb_url) %>% 
  pivot_longer(cols = c(awards, nominations), names_to = "result", values_to = "count")

# Generate the layout. sizetype can be area or radius, following your preference on what to be proportional to value.

packing_awards <- circleProgressiveLayout(oscars_long[oscars_long$result == "awards",]$count, 
                                          sizetype = "area")
data_awards <- bind_cols(oscars_long[oscars_long$result == "awards",], packing_awards) 
data_gg_awards <- circleLayoutVertices(packing_awards, npoints=100)

packing_nominations <- circleProgressiveLayout(oscars_long[oscars_long$result == "nominations",]$count, 
                                          sizetype = "area")
data_nominations <- bind_cols(oscars_long[oscars_long$result == "nominations",], packing_nominations) 
data_gg_nominations <- circleLayoutVertices(packing_nominations, npoints=100)



bubblechart <- function(data, data_gg) {
  data %>% 
    ggplot() + 
    geom_polygon(data = data_gg, 
                 aes(x, y, group = id, fill = as.factor(id)), 
                 colour = "grey70", size = 0.1, alpha = 0.6) +
    geom_textbox(aes(x, y, label = toupper(name)),
                 family = "Bebas Neue", fill = NA, box.colour = NA,
                 col = "white",
                 width = unit(2, "cm")) +
    scale_fill_manual(values = magma(nrow(data))) +
    scale_size_area(max_size = 5) +
    theme_void(base_family = "Bebas Neue") + 
    theme(legend.position = "none") +
    coord_equal() 
}

p1 <- bubblechart(data_awards, data_gg_awards)
p2 <- bubblechart(data_nominations, data_gg_nominations)

p1 + p2

ggsave(here("dontlookup", "oscars.png"), dpi = 300, width = 6, height = 6)


# https://www.flaticon.com/free-icon/oscar_206982?term=oscar%20academy%20award&page=1&position=3&page=1&position=3&related_id=206982&origin=tag
library(ggimage)

df_awards <- tibble(
  name = rep(oscars$name, oscars$awards),
  value = rep(oscars$awards, oscars$awards)
) %>% 
  group_by(name) %>% 
  mutate(name_row = row_number()) %>% 
  ungroup() %>% 
  mutate(name = factor(name, levels = c("Timothée Chalamet", "Mark Rylance",
                                        "Jonah Hill", "Jennifer Lawrence",
                                        "Leonardo di Caprio", "Cate Blanchett",
                                        "Meryl Streep")))

df_nominations <- tibble(
  name = rep(oscars$name, oscars$nominations),
  value = rep(oscars$nominations, oscars$nominations)
) %>% 
  group_by(name) %>% 
  mutate(name_row = row_number()) %>% 
  ungroup() %>% 
  mutate(name = factor(name, levels = c("Timothée Chalamet", "Mark Rylance", 
                                        "Jonah Hill", "Jennifer Lawrence",
                                        "Leonardo di Caprio", "Cate Blanchett",
                                        "Meryl Streep")))

bind_rows(df_awards, df_nominations, .id = "id") %>% 
  ggplot(aes(fct_reorder(name, value), name_row)) +
  geom_image(aes(image = here("dontlookup", "oscar.png")),
             color = "red") +
  coord_flip(ylim = c(0, 21)) +
  facet_wrap(vars(id)) +
  theme_minimal()



df_nominations %>% 
  ggplot(aes(fct_reorder(name, value), name_row)) +
  geom_image(aes(image = here("dontlookup", "oscar.png")), size = 0.1) +
  coord_flip(ylim = c(0, 21)) +
  theme_minimal()


sum(oscars$awards)
sum(oscars$nominations)

p <- df_nominations %>% 
  ggplot(aes(fct_reorder(name, value), name_row)) +
  geom_image(aes(image = here("dontlookup", "oscar.png")),
             color = "grey50", size = 0.05) +
  geom_image(data = df_awards,
             aes(image = here("dontlookup", "oscar.png")), size = 0.05) +
  geom_text(data = oscars,
            aes(x =  name, y = nominations, 
                label = glue("{nominations} ({awards})")), 
            nudge_y = 0.5, size = 2, hjust = 0, color = "grey60",
            family = "Roboto Condensed", fontface = "bold") +
  
  # Legend
  annotate("rect", xmin = 2.17, xmax = 3.33, ymin = 15.25, ymax = 20, fill = "grey96") +
  geom_image(data = NULL,
             aes(x = 3, y = 16, image = here("dontlookup", "oscar.png")),
              size = 0.05) +
  annotate("text", x = 3, y = 16.5, label = "Award",
           hjust = 0, family = "Roboto Condensed", size = 2.5, color = "grey40") + 
  geom_image(data = NULL,
             aes(x = 2.5, y = 16, image = here("dontlookup", "oscar.png")),
             color = "grey50", size = 0.05) +
  annotate("text", x = 2.5, y = 16.5, label = "Nomination",
           hjust = 0, family = "Roboto Condensed", size = 2.5, color = "grey40") + 
  
  scale_y_continuous(expand = c(0, 0), breaks = seq(5, 20, 5), position = "right") +
  coord_flip(xlim = c(0.25, NA), 
    ylim = c(0.5, 25)) +
  labs(title = "Don't Look Up Cast",
       subtitle = "41 nominations, 8 Academy Awards",
       caption = "Source: **IMDB.com** | Visualization: **Ansgar Wolsing**",
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    plot.title = element_text(color = "red", family = "Bebas Neue", size = 18),
    plot.subtitle = element_text(color = "red", family = "Roboto Condensed", 
                                 size = 11, margin = margin(b = 10)),
    plot.caption = element_markdown(size = 7, margin = margin(t = 0, b = 2),
                                    color = "grey50"),
    plot.title.position = "plot",
    panel.grid = element_blank(),
    axis.text.x.top = element_blank(),
    text = element_text(color = "grey35")
  )
ggsave(here("dontlookup", "dontlookup_oscar.png"), dpi = 300, width = 5, height = 4)
