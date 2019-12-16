library(shiny)
library(shinyjs)
source("R/helper-functions.R")

ui <- tagList(
  useShinyjs(),
  tags$head(includeCSS("custom.css")),
  navbarPage("timeline", 
             tabPanel("Main",
                      div(id = "timeline-date-select",
                          dateInput("user_date", "Please select how far to pull back",
                                    # value = lubridate::today() - lubridate::dweeks(3),
                                    # min = lubridate::today() - lubridate::dweeks(8),
                                    # max = lubridate::today()
                                    value = "1993-03-25",
                                    min = "1992-12-01",
                                    max = "1993-04-01"
                                    ),
                          actionButton("pull_data", label = NULL)
                      ),
                      shinyjs::hide(div(id = "timeline-content",
                                        plotOutput("plot1", width = "800px", height = "400px")
                      ))
             )))



server <- function(input, output, session) {
  observeEvent(input$user_date, {
    # updateActionButton(session, "pull_data", str_c("Pull data [", year_ww(input$user_date), " ~ ", year_ww(lubridate::today()), "]"))
    updateActionButton(session, "pull_data", str_c("Pull data [", year_ww(input$user_date), " ~ ", year_ww("1993-04-01"), "]"))
  })
  
  observeEvent(input$pull_data, {
    shinyjs::hide("timeline-date-select")
    shinyjs::show("timeline-content")
  })
  
  
}

shinyApp(ui, server)