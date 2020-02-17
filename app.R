#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

planets <- tribble(
    ~name, ~ra, ~dec, ~mag,
    "sun",  22,  -12, -26,
    "moon", 16.5,  -19, -10
)

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
    d/360*2*pi
}

visible_line <- function(lat, ha) {
    ha_rad  <- ha  %>% h_to_deg %>% deg_to_rad
    lat_rad <- lat %>% deg_to_rad
    a <- cos(ha_rad) * cos(lat_rad)
    b <- sin(ha_rad) * cos(lat_rad)
    c <-               sin(lat_rad)
    
    function(ra_deg) {
        ra <- ra_deg %>% deg_to_rad
        atan2(a*cos(ra)+b*sin(ra), c) %>% rad_to_deg
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

plot_stars <- function() {
    lat <- 42.3736
    ha  <- .5
    cambridge_visible <- is_visible(lat, ha)
    stars %>%
        top_n(3000, -mag) %>%
        filter(mag > -20) %>%
        ggplot(aes(x = h_to_deg(ra),
                   y = dec,
                   #color = cambridge_visible(ra, dec),
                   alpha = -mag)) +
        geom_point(size = .3) +
        # SUN
        stat_function(fun = visible_line(-12, 21.9),
                      geom = "point",
                      n = 1000,
                      size = .5,
                      color = "orange") +
        # VIEWER
        stat_function(fun = visible_line(lat, ha),
                      geom = "point",
                      size = .5,
                      n = 1000,
                      color = "black") +
        # ALWAYS
        stat_function(fun = ~(lat%%180-90),
                      geom = "point",
                      size = .1,
                      n = 1000,
                      color = "blue") +
        # EQUATOR
        stat_function(fun = ~0,
                      geom = "point",
                      size = .1,
                      n = 1000,
                      color = "red") +
        # NEVER
        stat_function(fun = ~(90-lat)%%180,
                      geom = "point",
                      size = .1,
                      n = 1000,
                      color = "blue") +
        #geom_hline(yintercept = c(lat%%180-90, 0, 90-lat%%180),
        #           linetype   = c("dotted", "solid", "dotted")) +
        annotate("point",
                 x = c(0, h_to_deg(ha), 0),
                 y = c(90, lat, -90),
                 size = 3,
                 shape = c("N", "X", "S"),
                 color = "red") +
        annotate("point",
                 x = h_to_deg(planets$ra),
                 y = planets$dec,
                 size = 3,
                 shape = c("O", "M"),
                 color = "red") +
        coord_map(projection = "rectangular",
                  lat0 = lat
                  #orientation=c(lat, -h_to_deg(ha), 0)
                  ) +
        scale_x_reverse() +
        theme(# plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "white"),
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            axis.text        = element_blank(),
            axis.ticks       = element_blank()) +
        guides(alpha=F, color = F) +
        labs(x = NULL, y = NULL)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
