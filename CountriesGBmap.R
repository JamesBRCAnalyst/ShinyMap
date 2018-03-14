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
gbcountries <- readOGR(dsn = file.path("Countries_December_2016_Full_Clipped_Boundaries_in_Great_Britain.shp"), stringsAsFactors = F)
nicountries <- readOGR(dsn = file.path("OSNI_Open_Data_Largescale_Boundaries__NI_Outline.shp"), stringsAsFactors = F)

#Prepare shapefile variables for merging
gbcountries <- subset(gbcountries, select = -c(objectid,ctry16cd,ctry16nmw,bng_e,bng_n,long,lat,st_areasha,st_lengths))
nicountries <- subset(nicountries, select = -c(ID,Area_SqKM,OBJECTID))
names(nicountries)[names(nicountries)=="NAME"] <- "ctry16nm"

#merge shapefiles
row.names(nicountries) <- paste("nicountries", row.names(nicountries), sep="_")
row.names(gbcountries) <- paste("gbcountries", row.names(gbcountries), sep="_")
gbcountries <- spTransform(gbcountries, CRS("+proj=longlat +datum=WGS84"))
nicountries <- spTransform(nicountries, CRS("+proj=longlat +datum=WGS84"))
ukcountries <- spRbind(gbcountries,nicountries)

#Import Excel file to add variables to locations
countrycalcs <- read_excel("regionalgvaperhead.xls", sheet = "Sheet2")

#merge variables from excel imported data to spatial dataframe. Matched by "CODE".
ukcountries <- merge(ukcountries, countrycalcs, by.x = "ctry16nm", by.y = "ctry16nm")

#Simplify map to reduce memory use ... WARNING: THIS DOESN'T WORK ON MY LAPTOP ... Good Luck!!!
gc()
ukcountries <- rmapshaper::ms_simplify(ukcountries)

#create colour palette for continuous colour variation (adjust number to how many intervals you want)
pal <- colorBin("Greens", ukcountries$GVAperhead, 4, pretty = TRUE)

#Create UI for ShinyApp
ui <- navbarPage("Countries", id="nav",
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
      addPolygons(data = ukcountries, color = ~pal(GVAperhead), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 1.0,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste("Country:", ukcountries$ctry16nm, "<br>", "GVA per head:", "£", prettyNum(ukcountries$GVAperhead, big.mark=",", scientific=FALSE), "<br>")) %>%
      addLegend("bottomright", pal = pal, values = ukcountries$GVAperhead,
                title = "GVA per head (£)",
                opacity = 1)
  })
  
  observe({})
  output$areatable <- DT::renderDataTable({
    ukcountries@data
    
  })
  
}

shinyApp(ui, server)
