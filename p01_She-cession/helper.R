library(tidyverse)
library(ggthemes)

Sys.setlocale("LC_TIME", "English")

windowsFonts(
  `Noto Serif` = windowsFont("Noto Serif"),
  `Noto Sans` = windowsFont("Noto Sans")
)


load_data <- function(filename, project_path) {
  readr::read_rds(stringr::str_c(project_path, "Data", filename, sep = "/"))
}

save_graph <- function(gg, filename, png = T, svg = F) {
  filename_png <- str_c(filename, ".png")
  filename_svg <- str_c(filename, ".svg")
  
  
  if(isTRUE(png)) {
    ragg::agg_png(
      filename = str_c(project_path, "Outputs", filename_png, sep = "/"),
      width = 9, height = 6, units = "in", res = 900
    )
    print(gg)
    dev.off()
  }
  
  if(isTRUE(svg)) {
    svglite::svglite(
      filename = str_c(project_path, "Outputs", filename_svg, sep = "/"),
      width = 9, height = 6
    )
    print(gg)
    dev.off()
  }
}

