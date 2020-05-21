library(shiny)
library(shinydashboard)
library(DT)

function(request) {
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "stRs"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
            fluidRow(
                box(
                    width = 3,
                    numericInput("lat",
                             "Latitude",
                             min = -90, max = 90,
                             value = 42.3736)
                    ),
                box(
                    width = 3,
                    numericInput("long",
                             "Longitude",
                             min = -180, max = 180,
                             value = -71.1097)
                ),
                box(
                    width = 3,
                    dateInput("time",
                             "Date (disabled)"
                             )
                ),
                box(
                    width = 3,
                    "The blue dots indicate what is always visible.
The red line is the equator.
The orange line is what is in the sun.
The X is treasure: you :)"
                )
            ),
            fluidRow(
                box(title = "Visible",
                    width = 6,
                    plotOutput("plot_up", height = "700px")
                ),
                box(title = "Hidden",
                    width = 6,
                    plotOutput("plot_down", height = "700px")
                )
            ),
            fluidRow(
                box(title = NULL,
                    width = 12,
                    plotOutput("plot_all", height = "1100px")
                )
            ),
            fluidRow(
                box(id = "planet_box",
                    title = "Object data",
                    collapsible = TRUE,
                    width = 12,
                    DT::dataTableOutput("planets")
                )
            )
        ),
        tags$head(tags$meta(property = "og:title",
                            content = "stRs"),
                  tags$title("stRs")
                  #tags$meta(property = "og:image",
                            #content = "https://plotypus.csail.mit.edu/plotypus.jpg")
                  )
    )
}
