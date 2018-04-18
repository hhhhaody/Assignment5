library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)

# Load data
a <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
b <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)

# no data for 2013, so we delete 2013 from the years list
years <- years[-26]
urls <- str_c(a, years, b, sep = "")
filenames <- str_c("mr", years, sep = "")

N <- length(urls)


for (i in 1:N){
  suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  if (i < 18) {
    file <- file %>% mutate(mm = "00")
  }
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  if (i >= 20) {
    file <- file[-1,]
  }
  if (i >= 26) {
    file[1] <- i + 1988
  }
  else{
    file[1] <- i + 1987
  }
  if(i == 1){
    total <- file
  }
  else{
    total <- rbind.data.frame(total, file)
  }
}
noon <- total %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00"))


noon$ATMP <- as.numeric(noon$ATMP)
noon$WTMP <- as.numeric(noon$WTMP)


noon$ATMP <- ifelse(noon$ATMP > 90, NA, noon$ATMP)
noon$WTMP <- ifelse(noon$WTMP > 90, NA, noon$WTMP)


noon <- unite(noon, Date, YYYY, MM, DD, sep = "-")
noon$Date <-as.Date(noon$Date)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "The Air & Sea Temperature"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series"),
      menuItem("Air & Sea Temp. Correlation", tabName = "correlation")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "time_series",
              fluidRow(
                box(selectInput("time_series_mode",
                                "Mode:",
                                choices = list("Air Temperature", "Sea Temperature",
                                               "Air and Sea Temperatures")), 
                    plotOutput("plot1"), width = 12)
              ),
              p("The above is a time series represenation of data collected for air and sea temperatures
                from NOAA's National Data Buoy Center at buoy 46035 located in the Bering Sea,
                except for the years 2012 and 2013, for which data was missing.")
      ),
      
      tabItem(tabName = "correlation",
              fluidRow(
                box(selectInput("cor_mode",
                                "Mode:",
                                choices = list("Scatter Plot", "Smooth Line")), 
                    plotOutput("plot2"), width = 12)
              ),
              p("we find ATMP and WTMP have coefficient of correlation of 0.877495. Therefore, we prove that ATMP and WTMP do have strong correlation")
      )
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$time_series_mode == "Air Temperature") {
      
      graph <- noon %>% ggplot(aes(Date, ATMP)) +
        geom_line(na.rm = TRUE, col = "red") +
        labs(title = "Time Series of Air Temperature (noon)",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature (Celcius)",
             x = "Year")
      print(graph)
    }
    
    if (input$time_series_mode == "Sea Temperature") {
      
      graph <- noon %>% ggplot(aes(Date, WTMP)) +
        geom_line(na.rm = TRUE, col = "blue") +
        labs(title = "Time Series of Sea Temperature (noon)",
             subtitle = "Data obtained from the National Data Buoy Center",
             y = "Temperature (Celcius)",
             x = "Year")
      print(graph)
    }     
    
    if (input$time_series_mode == "Air and Sea Temperatures") {
      
      graph <- ggplot(noon, aes(Date)) + 
        geom_line(aes(y = ATMP, col = "ATMP")) + 
        geom_line(aes(y = WTMP, col = "WTMP")) +
        scale_colour_manual(values=c("red", "blue")) +
        labs(x = "Year", y = "Temperature (Celcius Degree)",
             title = "Time Series of Air & Sea Temperature (Noon Data)",
             subtitle = "Data obtained from the National Data Buoy Center")
      print(graph)
    }     
    
  })
  
  
  output$plot2 <- renderPlot({
    if (input$cor_mode == "Scatter Plot") {
      
      graph <- ggplot(noon) + 
        geom_point(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Noon Air Temp (Celcius)", 
             y = "Noon Sea Temp (Celcius)",
             title = "Scatter Plot to See the Correlation between ATMP and WTMP")
      print(graph)
    }
    
    if (input$cor_mode == "Smooth Line") {
      
      graph <- ggplot(noon) + 
        geom_smooth(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Noon Air Temp (Celcius)", 
             y = "Noon Sea Temp (Celcius)",
             title = "Smooth Line to See the Correlation between ATMP and WTMP")
      print(graph)
    }
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
