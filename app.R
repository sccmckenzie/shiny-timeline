library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(kableExtra)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 6,
      dateInput("date", "Set mean timestamp", value = today()),
      numericInput("n", "# of events", 6, min = 1, max = 20),
      numericInput("spacing", "event spacing", 2, min = 1, max = 10),
      tableOutput("kable")
    ),
    mainPanel(
      width = 6,
      plotOutput("timeline", click = "timeline_click")
    )
  )
)

server <- function(input, output, session) {
  
  words <- readr::read_lines("words.txt")
  
  tbl <- reactive(make_events(input$date, input$n, input$spacing, words))
  
  tbl_timeline <- reactive({
    pivot_longer(tbl(), cols = c(t1, t2), names_to = "key1", values_to = "time1") %>% 
      select(-t3) %>% 
      left_join(pivot_longer(tbl(), cols = c(t1, t3), names_to = "key2", values_to = "time2") %>% select(-t2))
  })
  
  output$kable <- function() {
    tbl() %>% 
      rename(start = t1,
             finish = t2,
             extra = t3) %>% 
      knitr::kable("html") %>% 
      kable_styling("striped", full_width = FALSE, position = "left", font_size = 12) %>% 
      row_spec(clicked_row(), bold = T, color = "white", background = "#D7261E")
  }
  
  output$timeline <- renderPlot({
    tbl_timeline() %>% 
      ggplot() +
      geom_line(aes(time2, nickname, color = "extra time"), size = 10) +
      geom_line(aes(time1, nickname), size = 10, color = "#0a8280") +
      theme_minimal() +
      theme(text = element_text(size = 15),
            plot.title.position = "plot") +
      labs(title = "Click on event to highlight corresponding kable row", y = "Event Nickname", x = NULL)
  })
  
  clicked_row <- reactive({
    click_info <- input$timeline_click
    
    if (is.null(click_info)) return(numeric())
    
    which(pull(tbl(), nickname) == click_info$domain$discrete_limits$y[round(click_info$y, 0)])
  })
}

shinyApp(ui, server)