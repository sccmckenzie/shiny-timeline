library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(kableExtra)

words <- readr::read_lines("words.txt")

ui <- fluidPage(
  includeCSS("custom.css"),
  div(class = "inline",
      dateInput("date", "All events will more or less be calculated around selected Date", value = today()),
      numericInput("n", "# of data points", 6, min = 1, max = 20),
      numericInput("spacing", "event spacing", 2, min = 1, max = 10),
      tableOutput("kable")),
  div(class = "inline",
      plotOutput("timeline", width = "600px", height = "400px", click = "timeline_click")),
  div(class = "inline",
      verbatimTextOutput("click_info"))
)

server <- function(input, output, session) {
  
  
  make.events <- function(d, n, spacing) {
    t1 <- rnorm(n, mean = as_datetime(d, tz = "US/Central"), sd = dweeks(spacing)) %>% as_datetime(tz = "US/Central")
    t2 <- t1 + duration(rnorm(n, mean = dweeks(spacing / 2), sd = dweeks(spacing/ 4)))
    tibble(nickname = sample(words, n), t1, t2)
  }
  
  tbl <- reactive(make.events(input$date, input$n, input$spacing))
  
  output$kable <- function() {
    tbl() %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = FALSE, position = "left", font_size = 12)
  }
  
  output$timeline <- renderPlot({
    tbl() %>% 
      pivot_longer(cols = t1:t2, names_to = "key", values_to = "time") %>% 
      ggplot(aes(time, nickname)) +
      geom_line(size = 10, color = "#0a8280") +
      theme_minimal()
      

  })
  
  output$click_info <- renderPrint({
    nearPoints(tbl(), input$timeline_click)
  })
  
}

shinyApp(ui, server)