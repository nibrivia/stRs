
plot_stars <- function(lat = 42.3736, ha, globe = TRUE) {
    earth_tilt <- 23.43664
    cambridge_visible <- is_visible(lat, ha)
    objects <- objects()
    sun_ra  <- objects %>% filter(name == "Sun") %>% .[["ra"]]
    sun_dec <- objects %>% filter(name == "Sun") %>% .[["dec"]]
    
    plot <- stars %>%
        filter(mag > -20) %>%
        top_n(5000, -mag) %>%
        ggplot(aes(x = h_to_deg(ra),
                   y = dec,
                   #color = cambridge_visible(ra, dec),
                   alpha = -mag
                   )) +
        geom_point(size = .3,
                   color = "white") +
        
        # SUN
        stat_function(fun = ~(90-earth_tilt),
                      geom = "line", alpha = 0.2,
                      n = 72*2, color = "red") +
        stat_function(fun = ~(-90+earth_tilt),
                      geom = "line", alpha = 0.2,
                      n = 72*2, color = "red") +
        stat_function(fun = ~(-earth_tilt),
                      geom = "line", alpha = 0.2,
                      n = 72*2, color = "red") +
        stat_function(fun = ~(earth_tilt),
                      geom = "line", alpha = 0.2,
                      n = 72*2, color = "red") +
        stat_function(fun = visible_line(sun_dec, sun_ra %>% deg_to_h()),
                      geom = "line",
                      n = 72*5,
                      size = .3,
                      color = "orange") +
        
        #Ecliptic
        stat_function(fun = visible_line(90-earth_tilt, -90 %>% deg_to_h()),
                      geom = "line",
                      size = 0.1,
                      n = 72*50,
                      color = "cyan") +
        # ALWAYS
        stat_function(fun = ~((lat-180)%%180-90),
                      geom = "line",
                      size = 0.2,
                      n = 72*2,
                      linetype = "dashed",
                      color = "blue") +
        # EQUATOR
        stat_function(fun = ~0,
                      geom = "line",
                      size = .2,
                      n = 72*2,
                      color = "red") +
        # NEVER
        stat_function(fun = ~(180-lat)%%180-90,
                      geom = "line",
                      size = .2,
                      n = 72*2,
                      linetype = "dashed",
                      color = "blue") +
        # VIEWER
        stat_function(fun = visible_line(0.001, ha+90),
                      geom = "line", alpha = 0.15, n = 72*50,
                      color = "green") +
        stat_function(fun = visible_line(lat+90, ha),
                      geom = "line", alpha = 0.15, n = 72*50,
                      color = "green") +
        stat_function(fun = visible_line(lat, ha),
                      geom = "line", size = 0.1, n = 72*5,
                      color = "white") +
        #geom_hline(yintercept = c(lat%%180-90, 0, 90-lat%%180),
        #           linetype   = c("dotted", "solid", "dotted")) +
        annotate("text",
                 x = 360-90,
                 y = -earth_tilt,
                 vjust = "bottom", hjust = "left", size = 4, alpha = .5,
                 label = "Ecliptic (sun)",
                 color = "cyan") +
        annotate("text",
                 x = 358,
                 y = c(-90+earth_tilt, -earth_tilt, 0, earth_tilt, 90-earth_tilt),
                 vjust = "bottom", hjust = "left", size = 4, alpha = .5,
                 label = c("Antartic circle", "Tropic of Capricorn", "Equator", "Tropic of Cancer", "Arctic circle"),
                 color = "red") +
        annotate("text",
                 x = (h_to_deg(ha) + c(180, 90, 0, -90)) %% 360,
                 y = c(90-lat, 0, lat-90, 0),
                 vjust = c("inward", "", "inward", ""),
                 hjust = c("", "inward", "", "inward"),
                 size = 4,
                 label = c("N", "E", "S", "W"),
                 color = "green") +
        annotate("point",
                 x = c(0, h_to_deg(ha), 0),
                 y = c(90, lat, -90),
                 size = 3,
                 shape = c("N", "X", "S"),
                 color = c("red", "white", "red")) +
        annotate("point",
                 x = objects$ra,
                 y = objects$dec,
                 size = 1,
                 label = objects$name,
                 color = "white",
                 ) +
        annotate("text",
                 x = objects$ra,
                 y = objects$dec,
                 vjust = "inward", hjust = "inward",
                 size = 4,
                 label = objects$name,
                 color = "white",
                 ) +
        scale_x_reverse() +
        theme(plot.background = element_rect(fill = "black"),
            strip.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text        = element_blank(),
            axis.ticks       = element_blank()) +
        guides(alpha=F, color = F) +
        labs(x = NULL, y = NULL)
    
    if (globe) {
        plot <- plot + coord_map(projection = "orthographic", orientation=c(lat, -h_to_deg(ha), 0))
    } else {
        plot <- plot + coord_map(projection = "rectangular",  lat0 = lat)
    }
    
    plot
}
