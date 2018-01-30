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
library(magrittr)
library(leaflet)
library(htmltools)
library(tidyverse)
parks_tdy <- read_csv("https://raw.githubusercontent.com/carter-allen/VizUM/master/parks_tdy.csv")

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,selectInput("att","Select Attribute",
              list(
                "All" = "ALL",
                "Ampitheatre" = "AMPHITHEAT",
                "Auditorium" = "AUDITORIUM",
                "Archery" = "ARCHERY",
                "Baseball" = "BASEBALL",
                "Basketball" = "BASKETBALL",
                "Beach" = "BEACH",
                "Bike Trail" = "BIKETRAIL",
                "BMX Track" = "BMXTRACK",
                "Boat Ramp" = "BOATRAMP",
                "Campground" = "CAMPGROUND",
                "RV Campground" = "RVCAMPGROU",
                "Dog Park" = "DOGPARK",
                "Equestrian" = "EQUESTRIAN",
                "Football" = "FOOTBALL",
                "Frisbee Golf" = "FRISBEEGOL",
                "Golf" = "GOLF",
                "Driving Range" = "GOLFDRIVIN",
                "Gun Range" = "GUNRANGE",
                "Gymnasium" = "GYMNASIUM",
                "Marina" = "MARINA",
                "Mountain Biking" = "MOUNTAINBI",
                "Nature Center" = "NATURECENT",
                "Nature Trail" = "NATURETRAI",
                "Playground" = "PLAYGROUND",
                "Raquetball" = "RAQUETBALL",
                "Recreation Center" = "REC_CENTER",
                "Restroom" = "RESTROOM",
                "Roller Hockey" = "ROLLERHOCK",
                "Shelter" = "SHELTER",
                "Skating" = "SKATING",
                "Soccer" = "SOCCER",
                "Stadium" = "STADIUM",
                "Swimming" = "SWIMMING",
                "Tennis" = "TENNIC",
                "Track (400)" = "TRACK400",
                "Trail" = "TRAIL",
                "Volleyball" = "VOLLEYBALL",
                "Wet Playground" = "WETPLAYGRO",
                "WiFi" = "WIFI",
                "Zoo" = "ZOO")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  
  #parks <- reactive(cbind(parks,attribute))
  
   output$map <- renderLeaflet({
     if(toupper(input$att) == "ALL")
     {
       leaflet(data = parks_tdy) %>%
         addTiles() %>%
         addCircleMarkers(~lon,~lat,
                          popup = ~paste(sep = "<br/>",name,ADDRESS,PHONE,
                                         paste(sep = "","<b><a href=\'",PARKURL,"\'>Website</a></b>")),
                          label = ~htmlEscape(name),
                          radius = ~ifelse(park_size == "Small Park",3,
                                           ifelse(park_size == "Medium Park",6,
                                                  ifelse(park_size == "Large Park",9,12))),
                          stroke = FALSE,
                          fillOpacity = 0.50)
     }
     else{
       attribute <- select(parks_tdy,toupper(input$att))
       parks <- cbind(parks,attribute)
       parks_a <- parks[attribute == "Yes",]
       leaflet(data = parks_a) %>%
         addTiles() %>%
         addCircleMarkers(~lon,~lat,
                          label = ~htmlEscape(paste(name,":",TYPE,CLASS)),
                          radius = ~ifelse(park_size == "Small Park",3,
                                           ifelse(park_size == "Medium Park",6,
                                                  ifelse(park_size == "Large Park",9,12))),
                          stroke = FALSE,
                          fillOpacity = 0.50)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

