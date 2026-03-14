library(ggplot2)

# Get digits of PI
pi_url <- "https://www.piday.org/wp-json/millionpi/v1/million?action=example_ajax_request&page=1"
pi_million <- readr::read_file(pi_url)
pi_million_arr <- strsplit(pi_million, split = "")[[1]]
pi_million_arr <- as.numeric(pi_million_arr)
pi_million_arr <- as.character(pi_million_arr[!is.na(pi_million_arr)])
head(pi_million_arr)

# Define spiral properties
n <- 1000
circles <- 24
theta <- seq(0, circles * 2 * pi, length.out = n)
p <- 3

r <- theta^p
x <- r * cos(theta)
y <- r * sin(theta)

df_spiral <- data.frame(
  x = x, y = y, 
  digit = rev(pi_million_arr[seq_len(n)]),
  digit_weight = (seq_len(n) / n) ^ 5 *  n
)


p <- df_spiral |> 
  ggplot(aes(x, y)) +
  geom_text(
    aes(
      label = digit, 
      size = digit_weight,
      col = digit),
    family = "Roboto Mono",
    show.legend = FALSE
  ) +
  scale_x_reverse() +
  scale_size_continuous(range = c(0.1, 8)) +
  paletteer::scale_color_paletteer_d("yarrr::basel") +
  coord_equal() +
  labs(title = "Happy \U03C0 Day!") +
  theme_void(base_family = "Roboto Bold", base_size = 28, paper = "white") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave("pi-day.png", width = 5, height = 5)
