library(tidyverse)
source("plots.R")
source("data.R")
source("coordinates.R")

lat <- 42.3736
long <- -71.1097
ha <- ct2lst(long, "", date_to_jd(now(tz = "UTC")))

p <- plot_stars(lat = lat, ha = ha, globe = FALSE)
ggsave("nightsky-big.png",   plot = p, width = 265.3, height = 200, units = "mm", dpi = 80, scale = 4.5956, bg = "#000000")
ggsave("nightsky-small.png", plot = p, width = 265.3, height = 200, units = "mm", dpi = 80, scale = 2.2978, bg = "#000000")
