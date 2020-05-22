library(astrolibR)
library(lubridate)

stars <- read_csv("data/hygdata_v3.csv")

rlang::env_binding_unlock(baseenv(), "browser")
browser <<- list

date_to_vec <- function(date) {
    c(year(date), month(date), day(date), hour(date), minute(date)) 
}

date_to_jd <- function(date) {
    jdcnv(year(date), month(date), day(date),
          hour(date) + minute(date)/60 + second(date)/3600)
}

planets <- function(date = now()) {
    visible_planets <- c("Mercury", "Venus", "Mars",
                        "Jupiter", "Saturn")
    
    array_vec <- date_to_vec(date)
    planet_coords(array_vec, visible_planets) %>% 
        as_tibble() %>% 
        mutate(name = visible_planets,
               ra = as.numeric(ra),
               dec = as.numeric(dec))
}

objects <- function(date = now()) {
    planets <- planets(date)
    sun <- sunpos(date_to_jd(date)) %>% 
        as_tibble() %>% 
        mutate(name = "Sun") %>% 
        select(name, ra, dec)
    moon <- moonpos(date_to_jd(date)) %>% 
        as_tibble() %>% 
        mutate(name = "Moon") %>% 
        select(name, ra, dec)
    
    bind_rows(sun, moon, planets)
}
