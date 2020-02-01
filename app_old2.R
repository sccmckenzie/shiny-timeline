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
                                                              choices = NULL)),
                                              div(style="display: inline-block;vertical-align:top; width: 200px;",
                                                  selectInput(inputId = "intlk_ww2", "End WW",
                                                              choices = NULL))),
                                          div(class = "timeline-container",
                                              plotOutput("plot1", width = "800px", height = "400px")),
                                          actionLink("repull", "Re-pull data")
                      ))
             ), tabPanel("Test",
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum")
             )
  )
)



server <- function(input, output, session) {
  observeEvent(input$user_date, {
    # updateActionButton(session, "pull_data", str_c("Pull data [", year_wk(input$user_date), " ~ ", year_wk(lubridate::today()), "]"))
    updateActionButton(session, "pull_data", str_c("Pull data [", year_wk(input$user_date), " ~ ", year_wk("1993-04-01"), "]"))
  })

  observeEvent(input$pull_data, {
    shinyjs::hide(id = "timeline-date-select")
    shinyjs::show(id = "timeline-content")
  })
  
  observeEvent(input$pull_data, {
    updateSelectInput(session, "intlk_ww1",
                      choices = ww_choices(input$user_date),
                      selected = head(ww_choices(input$user_date), 1))
    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices(input$user_date),
                      selected = tail(ww_choices(input$user_date), 1))
  })

  observe({
    x <- input$intlk_ww1

    updateSelectInput(session, "intlk_ww2",
                      choices = ww_choices(input$user_date)[ww_choices(input$user_date) >= x],
                      selected = tail(ww_choices(input$user_date), 1))
  })
  
  observeEvent(input$repull, {
    shinyjs::hide(id = "timeline-content")
    shinyjs::show(id = "timeline-date-select")
  })
  
  
  
  data <- eventReactive(input$pull_data, {
    pull.data(input$user_date)
  })
  
  output$plot1 <- renderPlot({
    p1(data(), input$intlk_ww1, input$intlk_ww2)
  })
  
}

shinyApp(ui, server)