#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)
library(leaflet)
library(dplyr)
library(RColorBrewer)




server <- function(input, output) {
  
  linkNYC <- read.csv("linkNYC.csv")
  
  #get NYC map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/v4/mapbox.streets/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoiZnJhcG9sZW9uIiwiYSI6ImNpa3Q0cXB5bTAwMXh2Zm0zczY1YTNkd2IifQ.rjnjTyXhXymaeYG6r2pclQ",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.958, lat = 40.801453, zoom = 12)
  })
  
  
  #add linkNYC location on Map
  
  output$linkmap <- renderPlot({
    theme_set(theme_bw(16))
    NYMap <- qmap("newyork", zoom = 14, color = "bw", legend = "topleft")
    NYMap +
      geom_point(aes(x = lon, y = lat, colour = 'red', size = 'grey'),
                 data = lnikNYC)
    NYMap +
      stat_bin2d(
        aes(x = lon, y = lat, colour = 'red', fill = 'grey'),
        size = .5, bins = 30, alpha = 1/2,
        data = linkNYC
      )
    
  }
  )
  
  # get route on map
  
  output$route <- renderPlot({
    legs_df <- route(
      input$current_location, input$chosen_link,
      alternatives = TRUE
    )
    qmap('newyork', zoom = 15, maptype = 'hybrid',
         base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
      geom_leg(
        aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
            colour = route),
        alpha = 3/4, size = 2, data = legs_df
      )+
      labs(x = 'Longitude', y = 'Latitude', colour = 'Route') + facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')
    
  })
  
  #chose linNYC
    
  status <- reactive({
    link <- ID
    if (input$construction == TRUE){
      link <- filter(link, construction == "CX Complete/Link Installed")
    }else {
      link <- filter(link, construction == "Not Ready for CX")
    }
  })
  
    
  observe({  
    pal1 <- colorFactor(palette()[-1], levels(linkNYC$construction))
    Radius1 <- 3
    if (input$addlinkNYC == TRUE&length(as.matrix(cdata())) != 0){
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = NULL, options = markerOptions(opacity = 0.9), popup = ~Name) %>%
        addCircleMarkers(data = cdata(), ~Long, ~Lat, radius = Radius1, stroke = FALSE, fillOpacity = 0.7, fillColor = pal1(cdata()[["construction"]])) %>%
        addLegend("bottomleft", pal=pal1, values=cdata()[["Offense"]], title="link",
                  layerId="colorLegend")
    }
    else {
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = ttype(), ~Long, ~Lat, icon = restroomIcon(), options = markerOptions(opacity = 0.9), 
                   popup = paste("*Name:", ttype()$Name, "<br>",
                                 "*Address:", ttype()$Address, "<br>")) #%>%
    }
  })
  
  
  
  # Show a circle at the given location
  
  
  show <- reactive({
    function(eventid, lat, lng) {
      leafletProxy("map") %>% addCircles(lng=lng,lat=lat, radius=input$circleR, fillColor="red",layerId = eventid, group="overlays")
    } %>%
      return()
  })
  
  
  # When mouseover, show a circle
  
  observe({
    leafletProxy("map") %>% clearGroup("overlays")
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    isolate({
      show()(event$id, event$lat, event$lng)
    })
  })
}



