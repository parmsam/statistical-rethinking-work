library(shiny)
library(dplyr)
ui <- fluidPage(
  plotOutput("plot"),
  actionButton(
    "toss",
    "Random globe toss",
    icon = icon("globe"),
    width = '100%'
  )
)

server <- function(input, output, session) {
  land_or_water <- reactive({
    options <- c("land", "water")
    choice <- sample(options, size = 1, replace = TRUE)
  }) %>% bindEvent(input$toss)
  
  density_vector <- reactiveVal(0:10)
  
  observeEvent( land_or_water(), {
    if (land_or_water() == "water") {
      result <- density_vector() * (0:10)
      density_vector(result)
    } else if (land_or_water() == "land") {
      result <- density_vector() * (10:0)
      density_vector(result)
    }
  })
  
  output$plot <- renderPlot({
    plot(density_vector(), xlab = "proportion water", ylab = "density",
         type = "b")
  })
}

shinyApp(ui, server)