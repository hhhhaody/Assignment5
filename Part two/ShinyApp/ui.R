# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
data <- read.csv(file = "E:/veg1.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Chemical treatments applied to crops"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select domain to plot
                    selectInput(inputId = "Domain", label = strong("Dormain"),
                                choices = unique(data$Domain),
                                selected = "Domain.Category"),
                    
                    # Select year range to be plotted
                    dateRangeInput("Year", strong("Year range"), start = "1990", end = "2016",
                                   min = "1990", max = "2016"),
                    
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
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://cfpub.epa.gov/ecotox/", "Source: ECOTOX Knowledge", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  Domain <- reactive({
    req(input$Year)
    validate(need(!is.na(input$year[1]) & !is.na(input$year[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$year[1] < input$year[2], "Error: Start date should be earlier than end date."))
    data %>%
      filter(
        Domain.Category == input$Domain.Category,
        Year > as.POSIXct(input$Year[1]) & Year < as.POSIXct(input$Year[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    veg.3 <- dplyr::rename(data, 
                           Geo = `Geo.Level`, 
                           Commodity = `Commodity`,
                           Data = `Data.Item`,
                           Category = `Domain.Category`)
    p<-ggplot(data = veg.3, mapping = aes(Data, Geo ), environment = parent.frame())
    print(p)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(Domain()$Year), y = Domain()$close, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)