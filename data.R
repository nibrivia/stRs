library(astrolibR)
library(lubridate)

stars <- read_csv("data/hygdata_v3.csv")

rlang::env_binding_unlock(baseenv(), "browser")
browser <<- list

date_to_vec <- function(date) {
    date <- with_tz(date, tzone = "UTC")
    c(year(date), month(date), day(date), hour(date), minute(date)) 
}

date_to_jd <- function(date) {
    date <- with_tz(date, tzone = "UTC")
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
    
    #sun_ra  <- sun[2, "ra"]
    #sun_dec <- sun[2, "dec"]
    #anti_solar["ra"] <- -anti_solar["ra"]
    return(sun)
}

objects <- function(date = now()) {
    earth_objects <- read_csv("objects.csv")
    planets <- planets(date)
    sun  <-  sun_trace(date)
    moon <- moon_trace(date)
    people <- earth_objects %>% 
        mutate(dec = lat,
               ra  = h_to_deg(ct2lst(long, "", date_to_jd(date)))) %>% 
        select(name, ra, dec)
    
    milky_way <- tibble(name = "Milky Way", dec = -29.00781, ra = h_to_deg(17.761122))
    
    bind_rows(sun, moon, planets, people, milky_way)
}
