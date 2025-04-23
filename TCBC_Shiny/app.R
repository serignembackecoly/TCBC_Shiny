# Les librairies
library(shiny)
library(raster)
library(shiny.semantic)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(sf)
library(gt)

# Define UI for application that draws a histogram
ui <- semanticPage(
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
