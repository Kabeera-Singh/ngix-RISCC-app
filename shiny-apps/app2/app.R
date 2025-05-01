library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("RShiny App 2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                 choices = c("iris", "mtcars", "diamonds")),
      selectInput("variable", "Select variable:", choices = NULL)
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output, session) {
  # Update variable choices based on selected dataset
  observe({
    dataset <- switch(input$dataset,
                     "iris" = iris,
                     "mtcars" = mtcars,
                     "diamonds" = diamonds)
    
    updateSelectInput(session, "variable", 
                     choices = names(dataset)[sapply(dataset, is.numeric)])
  })
  
  output$boxplot <- renderPlot({
    dataset <- switch(input$dataset,
                     "iris" = iris,
                     "mtcars" = mtcars,
                     "diamonds" = diamonds)
    
    ggplot(dataset, aes_string(y = input$variable)) + 
      geom_boxplot(fill = "lightgreen") +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$variable))
  })
}

shinyApp(ui = ui, server = server)