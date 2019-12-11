library(shiny)
source("R/helper-functions.R")

ui <- fluidPage(
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      selectInput(inputId = "intlk_ww1", "Begin WW",
                  choices = ww_choices())),
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      selectInput(inputId = "intlk_ww2", "End WW",
                  choices = ww_choices()))
)

server <- function(input, output, session) {
  ww_timer <- reactiveTimer(60000)
  currentww <- eventReactive(ww_timer(), {
    str_c("Current WW: ", year_ww(now()))
  })
  output$ww <- renderText(currentww())
  
  observeEvent(ww_timer(), {
    updateSelectInput(session, "intlk_ww1",
                      choices = ww_choices())
    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices())
  })
  
  observe({
    x <- input$intlk_ww1
    
    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices()[ww_choices() >= x])
  })
}

shinyApp(ui, server)