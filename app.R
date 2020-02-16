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
    "sun",  21.9,  -12, -26,
    "moon", 15.8,  -16, -10,
    "camb",  8.0,   42.3, 0
)

plot_stars <- function() {
    stars %>%
        top_n(3000, -mag) %>%
        filter(mag > -20) %>%
        ggplot(aes(x = ra,
                   y = dec,
                   color = -42.3*cos((ra-8)/24*2*pi) < dec, #((dec - (42.3))^2+(360*(ra-8)/24)^2) < 90 | dec > 90-42.3,
                   alpha = -mag)) +
        geom_point(size = 3) +
        annotate("point",
                 x = planets$ra,
                 y = planets$dec,
                 size = 3,
                 shape = c("S", "M", "C"),
                 color = "yellow") +
        scale_x_reverse() +
        theme(# plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text        = element_blank()) +
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
