#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# packages
library(shiny)
library(leaflet)
library(htmltools)
library(tidyverse)
parks_tdy <- read_csv("https://raw.githubusercontent.com/carter-allen/VizUM/master/parks_tdy.csv")

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     leaflet(data = parks_tdy) %>%
       addTiles() %>%
       addCircleMarkers(~lon,~lat,
                        label = ~htmlEscape(name),
                        radius = 6,
                        stroke = FALSE,
                        fillOpacity = 0.5)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

