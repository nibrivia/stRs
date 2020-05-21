# coordinate things


hms_to_dec <- function(h, m, s) {
    h + m/60 + s/60/60
}

h_to_deg <- function(h) {
    h/24*360
}

deg_to_h <- function(d) {
    d/360*24
}

rad_to_deg <-function(d) {
    d/2/pi*360
}

deg_to_rad <-function(d) {
    d/360*2*pi %% (2*pi)
}

visible_line <- function(lat, ha) {
    ha_rad  <- ha  %>% h_to_deg %>% deg_to_rad
    lat_rad <- lat %>% deg_to_rad
    a <- cos(ha_rad) * cos(lat_rad)
    b <- sin(ha_rad) * cos(lat_rad)
    c <-               sin(lat_rad)
    
    function(ra_deg) {
        ra <- (ra_deg) %>% deg_to_rad
        d <- atan2((a*cos(ra)+b*sin(ra)), -c) %>% rad_to_deg
        (d+90) %% 180 - 90
    }
}

is_visible <- function(lat, ha) {
    local_visible_line <- visible_line(lat, ha)
    function(ra_deg, dec) {
        if (lat > 0) {
            local_visible_line(ra_deg) < dec
        } else {
            local_visible_line(ra_deg) > dec
        }
    }
}

