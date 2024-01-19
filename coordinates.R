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

rad_to_deg <- function(d) {
    (d/2/pi*360) %% 360
}

rad_to_lat <- function(d) {
    (d/2/pi*360 + 180) %% 360 - 180
}

deg_to_rad <- function(d) {
    d/360*2*pi %% (2*pi)
}

xyz_to_latlong <- function(x, y, z) {
    lat <- asin(z)
    long <- atan2(y, x)
    
    c(lat, long)
}

latlong_to_xyz <- function(lat = 0, long = 0) {
    x <- cos(long) * cos(lat)
    y <- sin(long) * cos(lat)
    z <-             sin(lat)
    
    c(x, y, z)
}

perp_to_latlong <- function(lat, long, theta = 0) {
    perp_0 <- c((lat + pi/2) %% (2*pi), long)
    
    rot_xyz(latlong_to_xyz(perp_0[1], perp_0[2]), latlong_to_xyz(lat, long), theta) 
}

# https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
rot_xyz <- function(xyz, normal, theta) {
    x <- normal[1]
    y <- normal[2]
    z <- normal[3]
    
    u <- matrix(xyz, nrow = 3)
    cprod <- matrix(
        c( 0, -z,  y,
           z,  0, -x,
          -y,  x,  0),
        nrow = 3)
    
    rot_matrix <- cos(theta)*diag(3) + sin(theta)*cprod  + (1-cos(theta))*(normal %o% normal)
    
    rot_matrix %*% xyz
}

circle_at <- function(lat, long, angle_radius) {
    xyz <- latlong_to_xyz(lat, long)
    center <- xyz * cos(angle_radius)
    
    function(theta) {
        ll <- center + sin(angle_radius)*perp_to_latlong(lat, long, theta)
        xyz_to_latlong(ll[1], ll[2], ll[3])
    }
}

circle_df <- function(lat, long, theta) {
    fn <- circle_at(lat %>% deg_to_rad(), long %>% h_to_deg() %>% deg_to_rad(), theta)
    
    thetas <- seq(from = 0, to = 2*pi, length.out = 1000)
    thetas %>%
        map(fn) %>% 
        transpose() %>%
        `names<-`(c("lat", "long")) %>%
        as_tibble() %>%
        mutate(lat = as.numeric(lat) %>% rad_to_lat(),
               long = as.numeric(long) %>% rad_to_deg()) %>% 
        arrange(long)
}


# visible_line <- function(lat, ha) {
#     ha_rad  <- ha  %>% h_to_deg %>% deg_to_rad
#     lat_rad <- lat %>% deg_to_rad
#     a <- cos(ha_rad) * cos(lat_rad)
#     b <- sin(ha_rad) * cos(lat_rad)
#     c <-               sin(lat_rad)
#     
#     function(ra_deg) {
#         ra <- (ra_deg) %>% deg_to_rad
#         d <- atan2((a*cos(ra)+b*sin(ra)), -c) %>% rad_to_deg
#         (d+90) %% 180 - 90
#     }
# }

visible_line <- function(lat, ha) {
    ha_rad  <- ha %>% h_to_deg %>% deg_to_rad
    lat_rad <- lat %>% deg_to_rad
    
    rot_fn <- circle_at(lat_rad, ha_rad, pi/2)

    thetas <- seq(from = 0, to = 2*pi, length.out = 1000)
    thetas %>%
        purrr::map(rot_fn) %>% 
        transpose() %>%
        `names<-`(c("lat", "long")) %>%
        as_tibble() %>%
        mutate(lat = as.numeric(lat) %>% rad_to_lat(),
               long = as.numeric(long) %>% rad_to_deg()) %>% 
        arrange(long)
        
        #ggplot(aes(x = long, y = lat)) +
        #geom_point() + 
        #coord_map()
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

