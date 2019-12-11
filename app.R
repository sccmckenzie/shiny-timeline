library(shiny)
source("R/helper-functions.R")

ui <- fluidPage(
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      selectInput(inputId = "intlk_ww1", "Begin WW",
                  choices = ww_choices())),
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      selectInput(inputId = "intlk_ww2", "End WW",
                  choices = ww_choices())),
  br(),
  actionLink("pull.data", "Pull Data"),
  plotOutput("plot1")
)

server <- function(input, output, session) {
  ww_timer <- reactiveTimer(60000)

  observeEvent(ww_timer(), {
    updateSelectInput(session, "intlk_ww1",
                      choices = ww_choices(),
                      selected = tail(ww_choices(), 1))
    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices())
  })
  
  observe({
    x <- input$intlk_ww1

    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices()[ww_choices() >= x],
                      selected = tail(ww_choices(), 1))
  })
  
  p <- eventReactive(input$pull.data, {
    pull.data(input$intlk_ww1, input$intlk_ww2)
  })
  
  output$plot1 <- renderPlot({
    pluck(p(), "p1")
  })
}

shinyApp(ui, server)