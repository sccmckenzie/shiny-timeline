library(shiny)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  dateInput("date", "All events will more or less be calculated around selected Date", value = today()),
  renderPlot("timeline", width = "1000px", height = "1000px")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)