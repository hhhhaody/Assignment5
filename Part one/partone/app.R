# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
data <- read.csv("E:/AIR.TEMPERATURES.csv", header = TRUE)
data0<- read.csv("E:/SEA.TEMPERATURES.csv", header = TRUE)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("COMPARING AIR AND SEA TEMPERATURES"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select domain to plot
                    selectInput(inputId = "TIME", label = strong("TIME"),
                                choices = unique(data$TIME),
                                selected = "DURATION"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, GGPLOT, and reference
                  mainPanel(
                    plotOutput(outputId = "GGPLOT", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "http://ggplot2.tidyverse.org/reference/geom_bar.html", "Source: GGPLOT2", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  TIME <- reactive({
    req(input$Year)
    validate(need(!is.na(input$year[1]) & !is.na(input$year[2])))
    validate(need(input$year[1] < input$year[2]))
    data %>%
      filter(
        DURATION == input$DURATION,
        Year > as.POSIXct(input$Year[1]) & Year < as.POSIXct(input$Year[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$GGPLOT <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot<-ggplot(data = Sea.temp, mapping = aes(TIME, AVG ), environment = parent.frame())
    plot + geom_boxplot(mapping = aes(TIME))
    plot0<-ggplot(data = Sea.temp, mapping = aes(TIME, AVG ), environment = parent.frame())
    plot0 + geom_boxplot(mapping = aes(TIME))
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(TIME()$Year), y = TIME()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
      
  