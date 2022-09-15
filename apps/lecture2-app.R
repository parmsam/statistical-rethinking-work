library(shiny)
library(dplyr)
library(bslib)
set.seed(1010)
ui <- fluidPage(
  align = "center",
  theme = bs_theme(version = 5, bootswatch = "litera"),
  textOutput("obs"),
  plotOutput("plot1"),
  br(),
  actionButton(
    "toss",
    "random globe toss",
    icon = icon("globe"),
    width = '100%',
    class = "btn-primary"
  ),
  br(),
  br(),
  actionButton(
    "reset",
    "reset",
    icon = icon("hourglass-start"),
    class = "btn-secondnary"
  )
)

server <- function(input, output, session) {
  land_or_water <- reactive({
    options <- c("L", "W")
    choice <- sample(options, size = 1, replace = TRUE)
  }) %>% bindEvent(input$toss)
  
  density_vector <- reactiveVal(rep(10, 11)*10:0)
  choices <- reactiveVal("L")
  
  observeEvent(land_or_water(), {
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
    density_vector(rep(10, 11)*10:0)
    choices("L")
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(3, 3, 0.25, 0.25))
    plot(
      scale(density_vector(), center = FALSE),
      xlab = "index",
      ylab = "density",
      type = "b",
      ylim=c(0,2.5)
    )
    mtext(side=2, line=2, "density", col="black", font=1, cex=1.2)
    mtext(side=1, line=2, "proportion water", col="black", font=1,cex=1.2)
  }, width = 500, height = 400)
  output$obs <- renderText({
    choices()
  })
}

shinyApp(ui, server)