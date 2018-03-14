#Set working directory so that all files remain together etc
setwd("C:/Users/**********")

#You might need most of these... It's best to load them just in case.
library(maptools)
library(ggmap)
library(shapefiles)
library(rgdal)
library(ggplot2)
library(RgoogleMaps)
library(foreign)
library(readxl)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(spdplyr)
library(raster)
library(lattice)
library(scales)
library(rJava)
library(js)
library(shinyjs)


#import shapefiles
ukregions <- readOGR(dsn = file.path("NUTS_Level_1_January_2018_Full_Extent_Boundaries_in_the_United_Kingdom.shp"), stringsAsFactors = F)

#Import Excel file to add variables to locations
calcs <- read_excel("regionalgvaperhead.xls", sheet = "Sheet1")

#merge variables from excel imported data to spatial dataframe. Matched by "CODE".
ukregions <- merge(ukregions, calcs, by.x = "CODE", by.y = "Code")

#create colour palette for continuous colour variation (adjust number to how many intervals you want)
pal <- colorBin("Greens", uk$PovertyNumberActual, 9, pretty = TRUE)

#Create UI for ShinyApp
ui <- navbarPage("Constituencies", id="nav",
                 tabPanel("Interactive map",
                          div(class="outer",
                              leafletOutput("mymap", height="700"),
                              tags$head(includeCSS("style.css"),
                                        includeScript("jscode.js")))),
                 tabPanel("Data Table",
                          DT::dataTableOutput("areatable")),
                 conditionalPanel("false", icon("crosshair"))
)

#Create Server Processes for Shiny App
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addPolygons(data = uk, color = ~pal(PovertyNumberActual), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 1.0,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste("Constituency:", uk$Area, "<br>", "In Poverty:", prettyNum(uk$PovertyNumberActual, big.mark=",", scientific=FALSE), "<br>")) %>%
      addLegend("bottomright", pal = pal, values = uk$PovertyNumberActual,
                title = "In Poverty",
                opacity = 1)
  })
  
  observe({})
  output$areatable <- DT::renderDataTable({
    uk@data %>%
      mutate(Action = paste('<a class="go-map" href="" data-Latitude="', Latitude, '" data-Longitude="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, uk@data)
    
    DT::datatable(uk@data, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
}

shinyApp(ui, server)
