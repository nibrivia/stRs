library(tidyverse)
source("plots.R")
source("data.R")
source("coordinates.R")

lat <- 42.3736
long <- -71.1097
ha <- ct2lst(long, "", date_to_jd(now(tz = "UTC")))

p <- plot_stars(lat = lat, ha = ha, globe = FALSE)
ggsave("nightsky.png", plot = p, width = 309, height = 174, units = "mm", dpi = 80, scale = 4)