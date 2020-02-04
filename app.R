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
      tableOutput("kable_summary")),
  div(class = "inline",
      plotOutput("timeline", width = "600px", height = "400px", click = "timeline_click")),
  div(class = "inline",
      tableOutput("kable_selected"))
)

server <- function(input, output, session) {
  
  
  make.events <- function(d, n, spacing) {
    t1 <- rnorm(n, mean = as_datetime(d, tz = "US/Central"), sd = dweeks(spacing)) %>% as_datetime(tz = "US/Central")
    t2 <- t1 + duration(rnorm(n, mean = dweeks(spacing / 2), sd = dweeks(spacing/ 4)))
    tibble(nickname = sample(words, n), t1, t2)
  }
  
  tbl <- reactive(make.events(input$date, input$n, input$spacing))
  
  tbl_timeline <- reactive({
    pivot_longer(tbl(), cols = t1:t2, names_to = "key", values_to = "time") %>% 
      mutate(nickname = factor(nickname))
  })
  
  output$kable <- function() {
    tbl() %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = FALSE, position = "left", font_size = 12)
  }
  
  output$timeline <- renderPlot({
    tbl_timeline() %>% 
      ggplot(aes(time, nickname)) +
      geom_line(size = 10, color = "#0a8280") +
      theme_minimal()
  })
  
  selected_event <- reactive({
    if (is.null(input$timeline_click$y)) return()
    
    levels(purrr::pluck(tbl_timeline(), "nickname"))[round(input$timeline_click$y)]
  })
  
  output$kable_selected <- function() {
    if (is.null(input$timeline_click$y)) return()
    
    tbl() %>% 
      filter(nickname == selected_event()) %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = FALSE, position = "left", font_size = 12)
  }
  
  
}

shinyApp(ui, server)