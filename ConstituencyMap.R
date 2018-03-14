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
gb <- readOGR(dsn = file.path("westminster_const_region.shp"), stringsAsFactors = F)
ni <- readOGR(dsn = file.path("OSNI_Open_Data_Largescale_Boundaries__Parliamentary_Constituencies_2008.shp"), stringsAsFactors = F)

#Prepare shapefile variables for merging
gb <- subset(gb, select = -c(NAME,AREA_CODE,DESCRIPTIO,FILE_NAME,NUMBER,NUMBER0,HECTARES,POLYGON_ID,UNIT_ID,TYPE_CODE,DESCRIPT0,TYPE_COD0,AREA,DESCRIPT1))
ni <- subset(ni, select = -c(PC_NAME,Area_sqkm,OBJECTID))
names(ni)[names(ni)=="PC_ID"] <- "CODE"

#merge shapefiles
row.names(ni) <- paste("ni", row.names(ni), sep="_")
row.names(gb) <- paste("gb", row.names(gb), sep="_")
gb <- spTransform(gb, CRS("+proj=longlat +datum=WGS84"))
ni <- spTransform(ni, CRS("+proj=longlat +datum=WGS84"))
uk <- spRbind(gb,ni)

#Import Excel file to add variables to locations
calcs <- read_excel("PayByConstituency.xls", sheet = "Final CALCS")

#simplify variables to appropriate decimal places etc... These will be directly used in table
calcs$FoodSpendMedian <- round(calcs$FoodSpendMedian, 2)
calcs$Earnings <- round(calcs$Earnings, 2)
calcs$PovertyProportion <- round(calcs$PovertyProportion, 2)
calcs$PovertyNumber <- round(calcs$PovertyNumber, 2)
calcs$PovertyNumberActual <- round(calcs$PovertyNumberActual, 0)
calcs$Longitude <- round(calcs$Longitude, 4)
calcs$Latitude <- round(calcs$Latitude, 4)
prettyNum(calcs$PovertyNumberActual, big.mark=",", scientific=FALSE)

#merge variables from excel imported data to spatial dataframe. Matched by "CODE".
uk <- merge(uk, calcs, by.x = "CODE", by.y = "Code")

### Map Making ###

#Simplify map to reduce memory use
gc()
uk <- rmapshaper::ms_simplify(uk)

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
    uk@data 
  })
  
}

shinyApp(ui, server)
