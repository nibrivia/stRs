
plot_stars <- function(lat = 42.3736, ha, globe = TRUE) {
    cambridge_visible <- is_visible(lat, ha)
    objects <- objects()
    sun_ra  <- objects %>% filter(name == "Sun") %>% .[["ra"]]
    sun_dec <- objects %>% filter(name == "Sun") %>% .[["dec"]]
    plot <- stars %>%
        top_n(1000, -mag) %>%
        filter(mag > -20) %>%
        ggplot(aes(x = h_to_deg(ra),
                   y = dec,
                   #color = cambridge_visible(ra, dec),
                   alpha = -mag
                   )) +
        geom_point(size = .3,
                   color = "white") +
        # SUN
        stat_function(fun = visible_line(sun_dec, sun_ra %>% deg_to_h()),
                      geom = "line",
                      n = 1000,
                      color = "orange") +
        # VIEWER
        stat_function(fun = visible_line(lat, ha),
                      geom = "line",
                      size = 0.1,
                      n = 10000,
                      color = "grey") +
        # ALWAYS
        stat_function(fun = ~((lat-180)%%180-90),
                      geom = "point",
                      size = .3,
                      n = 72*2,
                      color = "blue") +
        # EQUATOR
        stat_function(fun = ~0,
                      geom = "point",
                      size = .3,
                      n = 72*2,
                      color = "red") +
        # NEVER
        stat_function(fun = ~(180-lat)%%180-90,
                      geom = "point",
                      size = .3,
                      n = 72*2,
                      color = "blue") +
        #geom_hline(yintercept = c(lat%%180-90, 0, 90-lat%%180),
        #           linetype   = c("dotted", "solid", "dotted")) +
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
