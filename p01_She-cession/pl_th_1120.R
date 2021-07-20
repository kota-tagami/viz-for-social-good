background_color <- "#FAFAFA"
line_color <- "grey5"
text_color <- "grey5"

base_color <- "#56145B"
sub_color <- "#A86399"
accent_color <- "#D58F04"

base_font <- "Noto Serif"



bg <- function(color = NULL, ...) {
  element_rect(fill = background_color, color = color, ...)
}

ln <- function(size = 0.5, linetype = "dotted", ...) {
  element_line(color = line_color, size = size, linetype = linetype, ...)
}

txt <- function(size = 15, ...) {
  element_text(family = base_font, color = text_color, size = size, ...)
}



scale_date <- 
  seq.Date(as.Date("1990-01-01"), as.Date("2021-05-01"), by = "10 year")
cap_source <- 
  "Source: Labour Force Survey."
cap_notes <- 
  str_c(
    "Notes:",
    "Employees in non-agricultural sectors.",
    "The lighter line shows observed values,",
    "and the darker line is the smoothed trend (LOESS, span=0.65).",
    "The shadow indicates the period during COVID-19 pandemic.",
    sep = " "
  ) %>% 
  str_wrap(., width = 150, exdent = 0)



pl_th <- list(
  theme(
    plot.background = bg(),
    panel.background = bg(color = line_color, size = 0.8),
    strip.background = bg(),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = ln(size = 0.8),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.ticks.x = ln(size = 0.8, linetype = "solid"),
    axis.ticks.y = element_blank(),
    
    axis.title = txt(),
    axis.text = txt(),
    strip.text = txt(),
    
    plot.caption.position = "plot",
    plot.caption = txt(size = 10, hjust = 0),
  ),
  scale_x_date(
    breaks = scale_date,
    date_labels = "%Y"
  ),
  scale_y_continuous(
    breaks = seq(1, 100, 2)
  ),
  labs(
    x = NULL, y = "Million",
    caption = str_c(cap_source, "\n", cap_notes)
  ),
  annotation_raster(
    alpha(accent_color, 0.5),
    xmin = as.Date("2020-03-01"), xmax = as.Date("2021-07-01"),
    ymin = -Inf, ymax = Inf
  )
)


pl_th_facet_dt <- 
  tibble(
    gender = factor(c(rep("Women", 2), rep("Men", 2))), 
    value = c(22, 30, 30, 38)
  ) %>% 
  mutate(
    date = as.Date("2020-05-01"),
  )


geoms_all <- list(
  geom_point(
    data = pl_th_facet_dt,
    aes(x = date, y = value),
    alpha = 0
  ),
  geom_line(
    color = sub_color,
    size = 0.8
  ),
  geom_smooth(
    method = "loess", 
    formula = y ~ x,
    se = F, span = 0.65,
    color = base_color,
    size = 1
  ),
  facet_wrap(
    ~ gender + age, 
    scales = "free_y", ncol = 1,
    labeller = labeller(
      age = function(x) str_c("aged", x, sep = " "),
      .multi_line = F
    )
  )
)


geoms_gen <- list(
  geom_line(
    color = sub_color,
    size = 0.8
  ),
  geom_smooth(
    method = "loess", 
    formula = y ~ x,
    se = F, span = 0.65,
    color = base_color,
    size = 1
  ),
  facet_wrap(
    ~ gender + age, 
    scales = "free_y",
    labeller = labeller(
      age = function(x) str_c("aged", x, sep = " "),
      .multi_line = F
    )
  )
)


geoms_year <- list(
  geom_line(
    color = sub_color,
    size = 0.8
  ),
  geom_smooth(
    method = "loess", 
    formula = y ~ x,
    se = F, span = 0.65,
    color = base_color,
    size = 1
  ),
  facet_wrap(
    ~ gender + age, 
    scales = "free_y",
    labeller = labeller(
      age = function(x) str_c("aged", x, sep = " "),
      .multi_line = F
    )
  )
)

