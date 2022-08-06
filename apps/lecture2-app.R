library(shiny)
library(dplyr)
library(bslib)

ui <- fluidPage(
  align = "center",
  theme = bs_theme(version = 5, bootswatch = "readable"),
  textOutput("obs"),
  plotOutput("plot"),
  br(),
  actionButton(
    "toss",
    "Random globe toss",
    icon = icon("globe"),
    width = '100%',
    class = "btn-primary"
  ),
  actionButton("reset", "Reset",icon = icon("hourglass-start"), class = "btn-secondnary")
)

server <- function(input, output, session) {
  land_or_water <- reactive({
    options <- c("L", "W")
    choice <- sample(options, size = 1, replace = TRUE)
  }) %>% bindEvent(input$toss)
  
  density_vector <- reactiveVal(10:0)
  choices <- reactiveVal("L")
  
  observeEvent( land_or_water(), {
    prevChoices <- choices()
    choices(paste0(prevChoices, land_or_water()))
    if (land_or_water() == "W") {
      result <- density_vector() * (0:10)
      density_vector(result)
    } else if (land_or_water() == "L") {
      result <- density_vector() * (10:0)
      density_vector(result)
    }
  })
  
  observeEvent(input$reset, {
    density_vector(10:0)
    choices("L")
  })
  
  output$plot <- renderPlot({
    par(mar = c(2, 2, 0.25, 0.25)) 
    plot( scale(density_vector(), center = FALSE), xlab = "proportion water", ylab = "density",
         type = "b")
  })
  output$obs <- renderText({
    choices()
  })
}

shinyApp(ui, server)