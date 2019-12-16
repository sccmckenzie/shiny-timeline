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
                      shinyjs::hidden(div(id = "timeline-content",
                                          div(class = "timeline-container",
                                              div(style="display: inline-block;vertical-align:top; width: 200px;",
                                                  selectInput(inputId = "intlk_ww1", "Begin WW",
                                                              choices = ww_choices())),
                                              div(style="display: inline-block;vertical-align:top; width: 200px;",
                                                  selectInput(inputId = "intlk_ww2", "End WW",
                                                              choices = ww_choices()))),
                                          div(class = "timeline-containter",
                                              plotOutput("plot1", width = "800px", height = "400px")),
                                          actionLink("repull", "Re-pull data")
                      ))
             )))



server <- function(input, output, session) {
  observeEvent(input$user_date, {
    # updateActionButton(session, "pull_data", str_c("Pull data [", year_ww(input$user_date), " ~ ", year_ww(lubridate::today()), "]"))
    updateActionButton(session, "pull_data", str_c("Pull data [", year_ww(input$user_date), " ~ ", year_ww("1993-04-01"), "]"))
  })

  observeEvent(input$pull_data, {
    shinyjs::hide(id = "timeline-date-select")
    shinyjs::show(id = "timeline-content")
  })
  
  observeEvent(input$repull, {
    shinyjs::hide(id = "timeline-content")
    shinyjs::show(id = "timeline-date-select")
  })
  
  data <- eventReactive(input$pull_data, {
    pull.data(input$user_date)
  })
  
  output$plot1 <- renderPlot({
    p1(data())
  })
  
}

shinyApp(ui, server)