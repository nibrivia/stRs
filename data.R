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

planets <- function(date) {
    visible_planets <- c("Mercury", "Venus", "Mars",
                        "Jupiter", "Saturn")
    
    array_vec <- date_to_vec(date)
    planet_coords(array_vec, visible_planets) %>% 
        as_tibble() %>% 
        mutate(name = visible_planets,
               ra = as.numeric(ra),
               dec = as.numeric(dec))
}


moon_trace <- function(date) {
    moon <- moonpos(date_to_jd(date + hours((-1:4)*6))) %>% 
        as_tibble() %>% 
        mutate(name = "") %>% 
        select(name, ra, dec)
    moon[2, "name"] <- "Moon"
    return(moon)
}

sun_trace <- function(date) {
    sun <- sunpos(date_to_jd(date + days(-1:7))) %>% 
        as_tibble() %>% 
        mutate(name = "") %>% 
        select(name, ra, dec)
    sun[2, "name"] <- "Sun"
    return(sun)
}

objects <- function(date = now(tz="UTC")) {
    earth_objects <- read_csv("objects.csv")
    planets <- planets(date)
    sun  <-  sun_trace(date)
    moon <- moon_trace(date)
    people <- people %>% 
        mutate(dec = lat,
               ra  = h_to_deg(ct2lst(long, "", date_to_jd(date)))) %>% 
        select(name, ra, dec)
    
    bind_rows(sun, moon, planets, people, earth_objects)
}
