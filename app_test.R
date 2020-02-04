library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    plotOutput("plot1", height = 300, width = 300,
               click = "plot1_click",
    )
  ),
  verbatimTextOutput("x_value"),
  verbatimTextOutput("selected_rows")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(ToothGrowth, aes(supp)) + geom_bar()
  })
  
  # Print the name of the x value
  output$x_value <- renderPrint({
    if (is.null(input$plot1_click$x)) return()
    
    lvls <- levels(ToothGrowth$supp)
    lvls[round(input$plot1_click$x)]
  })
  
  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
    if (is.null(input$plot1_click$x)) return()
    
    keeprows <- round(input$plot1_click$x) == as.numeric(ToothGrowth$supp)
    ToothGrowth[keeprows, ]
  })
}

shinyApp(ui, server)