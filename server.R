library(tidyverse)
library(shiny)
source("data.R")
source("coordinates.R")
source("plots.R")


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$planets <- DT::renderDataTable({
        planets() %>% 
            select(name, ra, dec)
        }, 
        options = list(pageLength = 15, dom = "ftp"),
        rownames = FALSE)
    
    ha <- reactive({
        ct2lst(input$long, "", date_to_jd(now(tz = "UTC")))
        
    })

    output$plot_all <- renderPlot({
        plot_stars(lat = input$lat, ha = 0, globe = FALSE)
    })
    
    output$plot_up <- renderPlot({
        plot_stars(lat = input$lat, ha = 0)
    })
    
    output$plot_down <- renderPlot({
        plot_stars(lat = -input$lat, ha = 12)
    })
}
