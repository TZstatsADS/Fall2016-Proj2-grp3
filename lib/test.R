library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(ggmap)

ui <- dashboardPage(
  skin='yellow',
  dashboardHeader(title = "Parking Aide",
                  titleWidth = 300,
                  dropdownMenuOutput("notificationMenu"),
                  dropdownMenu(type = "messages",
                               badgeStatus = NULL, icon=icon("wrench"),
                               messageItem(
                                 from = "Developers",
                                 message = "Yaqing, Zichen, Kyongmook, Jennifer"
                               ),
                               messageItem(
                                 from = "Version",
                                 message = "Parking Aide 1.0",
                                 icon = icon("question-circle-o")
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "Applied Data Science",
                                 icon = icon("cog")
                               )
                  )
                  
  ),
  
  dashboardSidebar(
                   width = 300,
                   
                   sidebarMenu(
                     id='sidebarmenu',
                     menuItem("Park My Car",tabName="parktab",icon=icon("car"),
                               menuSubItem(icon=NULL, tabName="parktab", selectInput("price","Price",c('Free','Paid'))),
                               menuSubItem(icon=NULL, tabName="parktab", uiOutput("pricerange")),
                                           
                               menuSubItem(icon=NULL, tabName="parktab", sliderInput("distance","Max dist from your location (mi)",
                                                       min = 1, max = 20, value = 1)),
                                           
                               menuSubItem(icon=NULL, tabName="parktab", dateRangeInput("daterange", "Date range:",
                                              start  = Sys.Date(),
                                              end    = Sys.Date(),
                                              min    = Sys.Date(),
                                              max    = "2100-12-31",
                                              format = "mm/dd/yy",
                                              separator = " to ")),
                                           
                               menuSubItem(icon=NULL, tabName="parktab", uiOutput("hourrange")),
                               menuSubItem(tabName="park", actionButton("submit", "Find My Parking Spot", icon("paper-plane"),
                                                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                              ),
                     menuItem("Nearby Facilities",tabName="facilitiestab",icon=icon("dashboard"),
                              menuSubItem(icon=NULL, tabName="facilitiestab", 
                                          selectInput("facility","Facility",c('Gas Station','Restroom'))),
                              menuSubItem(tabName="facilitiestab", actionButton("submit2", "Find Facilities", icon("paper-plane"),
                                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                              )
                     
                     )
  ),
  
  dashboardBody(
              tabItems(tabItem(tabName = "parktab",
                       h2(leafletOutput("parkingmap"),"top spots: ")),
                       tabItem(tabName = "facilitiestab",
                       h2(leafletOutput("facilitymap")))         
               )
  )
)



server <- function(input, output, session) {
  #reactive data
  
  output$parkingmap <- renderLeaflet({
    leaflet() %>% setView(lng = -73.9712, lat = 40.7831, zoom = 14) %>% addTiles() %>% addProviderTiles("CartoDB.Positron")
  })

  output$facilitymap <- renderLeaflet({
    leaflet() %>% setView(lng = -73.9712, lat = 40.7831, zoom = 14) %>% addTiles() %>% addProviderTiles("CartoDB.Positron")
  })
  
  observeEvent(input$parkingmap_click, {
    click<-input$parkingmap_click
    clat <- click$lat
    clng <- click$lng
    address <- revgeocode(c(clng,clat))
    leafletProxy('parkingmap') %>% clearShapes()  %>%
      addCircles(lng=clng, lat=clat, group='circles',
                 weight=1, radius=50, color='black', fillColor='orange',
                 popup=address, fillOpacity=0.5, opacity=1)
  })
  
  output$pricerange <- renderUI({
    if(input$price=="Free")
      return()
    sliderInput("pricerange", label="willing to pay ($/hr):", min=0, max=50, value=c(0,50), step = 0.1)
  })

  output$hourrange <- renderUI({
    sliderInput("hourrange", label = "Hour range",
                min = as.POSIXct(paste(input$daterange[1], " 00:00:00")),
                max = as.POSIXct(paste(input$daterange[2]," 23:59:59")),
                value = c(
                  as.POSIXct(paste(input$daterange[1], " 00:00:00")),
                  as.POSIXct(paste(input$daterange[2]," 23:59:59"))
                ))
  })
  
  output$notificationMenu <- renderMenu({
    ###
    record <- c("Record 1", "Record 2", "Record 3", "Record 4", "Record 5")
    historicalData <- data.frame(record)
    his <- apply(historicalData, 1, function(row) {
      notificationItem(text = row[["record"]], icon=icon("clock-o"))
    })
    
    dropdownMenu(type = "notifications", badgeStatus = NULL, icon=icon("history"), .list=his)
  })

  observeEvent(input$facilitymap_click, {
    click2<-input$facilitymap_click
    clat2 <- click2$lat
    clng2 <- click2$lng
    address2 <- revgeocode(c(clng2,clat2))
    leafletProxy('facilitymap') %>% clearShapes()  %>%
      addCircles(lng=clng2, lat=clat2, group='circles',
                 weight=1, radius=50, color='black', fillColor='green',
                 popup=address2, fillOpacity=0.5, opacity=1)
  })
  
  observeEvent(input$submit2, {
    if(is.null(address2))
      return()
    facilitychosen <- input$facility
  })
  
  distance_calculation <- function (lat1, lon1, lat2, lon2) {
    radlat1 = pi * lat1/180
    radlat2 = pi * lat2/180
    theta = lon1-lon2
    radtheta = pi * theta/180
    dist = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(radtheta);
    dist = acos(dist)
    dist = dist * 180/pi
    dist = dist * 60 * 1.1515
    dist = dist * 0.8684
    return(dist)
  }
  
  facilitydata <- reactive({
    if(is.null(facilitychosen))
      return()
    if(facilitychosen == "Gas Station")
      facilitydata <- read.csv("../data/Gas Station in Manhattan.csv")
      facilitydata <- facilitydata[]
      kj
  })
  
}

shinyApp(ui, server)
