library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinyjs)
library(ggmap)

ui <- dashboardPage(
  skin='yellow',
  dashboardHeader(title = "Parking Aide",
                  titleWidth = 300,
                  dropdownMenu(type = "messages",
                               badgeStatus = NULL, icon=icon("wrench"),
                               messageItem(
                                 from = "Developers",
                                 message = "Yaqing, Kyongmook, Zichen"
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
                                                       min = 0, max = 5, step = 0.1, value = 1)),
                                           
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
                                          selectInput("facility","Facility",c('Gas Station', 'Garage', 'Restroom'))),
                              menuSubItem(icon=NULL, tabName="facilitiestab", sliderInput("distance2","Max dist from your location (mi)",
                                                                                          min = 1, max = 20, value = 1)),
                              menuSubItem(tabName="facilitiestab", actionButton("submit2", "Find Facilities", icon("paper-plane"),
                                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                              )
                     
                     )
  ),
  
  dashboardBody(
              tabItems(tabItem(tabName = "parktab",
                       h2(leafletOutput("parkingmap"),
                          div(DT::dataTableOutput("outputtable"), style = "font-size:50%"),
                          div(DT::dataTableOutput("outputtable1"), style = "font-size:50%"))),
                       tabItem(tabName = "facilitiestab",
                       h2(leafletOutput("facilitymap"),div(DT::dataTableOutput("outputtable2"), style = "font-size:50%"))
                       )         
               )
  )
)



server <- function(input, output, session) {
  ### UI: define two maps ###
  output$parkingmap <- renderLeaflet({
    leaflet() %>% setView(lng = -73.9712, lat = 40.7831, zoom = 14) %>% addTiles() %>% addProviderTiles("CartoDB.Positron")
  })

  output$facilitymap <- renderLeaflet({
    leaflet() %>% setView(lng = -73.9712, lat = 40.7831, zoom = 14) %>% addTiles() %>% addProviderTiles("CartoDB.Positron")
  })
  
  ### Add current location on maps ###
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
  
  ### UI: provide price range input if user chooses "paid"
  output$pricerange <- renderUI({
    if(input$price=="Free")
      return()
    sliderInput("pricerange", label="willing to pay ($/hr):", min=0, max=50, value=c(0,50), step = 0.1)
  })
  
  ### UI: provide hour range based on date input
  output$hourrange <- renderUI({
    sliderInput("hourrange", label = "Hour range",
                min = as.POSIXct(paste(input$daterange[1], " 00:00:00")),
                max = as.POSIXct(paste(input$daterange[2]," 23:59:59")),
                value = c(
                  as.POSIXct(paste(input$daterange[1], " 00:00:00")),
                  as.POSIXct(paste(input$daterange[2]," 23:59:59"))
                ))
  })

  
  ### Customize marker icons
  greenparkingLeafIcon <- makeIcon(
    iconUrl = "/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/lib/parking_icon_green.png",
    iconWidth = 30
  )
  
  redparkingLeafIcon <- makeIcon(
    iconUrl = "/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/lib/parking_icon_red.png",
    iconWidth = 30
  )
  
  yellowparkingLeafIcon <- makeIcon(
    iconUrl = "/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/lib/parking_icon_yellow.png",
    iconWidth = 30
  )
  
  gasstationLeafIcon <- makeIcon(
    iconUrl = "https://cdn3.iconfinder.com/data/icons/map/500/gasstation-512.png",
    iconWidth = 25
  )
  
  garageLeafIcon <- makeIcon(
    iconUrl = "https://cdn4.iconfinder.com/data/icons/car-soft/512/car_transport_traffic_auto_transportation_vehicle_automobile_v2-512.png",
    iconWidth = 25
  )
  
  restroomLeafIcon <- makeIcon(
    iconUrl = "https://cdn3.iconfinder.com/data/icons/map-markers-1/512/toilet-512.png",
    iconWidth = 25
  )
  
  ### Define reactive tables
  values <- reactiveValues(facilitytable = NULL, parkingtable = NULL, parkingtable1 = NULL)
  output$outputtable2 <- DT::renderDataTable(values$facilitytable[,c(1,2,5)], 
                                             rownames=FALSE)
  output$outputtable <- DT::renderDataTable(values$parkingtable[,c(1,4:10)], 
                                            rownames=FALSE)
  output$outputtable1 <- DT::renderDataTable(values$parkingtable1[,c(1,2,6,7,8)], 
                                            rownames=FALSE)
  
  
  
  ### Add markers on Parking map
  observeEvent(input$submit, {
    if(is.null(input$parkingmap_click))
      return()
    click1 <- input$parkingmap_click
    clat1 <- click1$lat
    clng1 <- click1$lng
    maxdist <- input$distance
    starttime <- input$hourrange[1]
    endtime <- input$hourrange[2]
    startday <- weekdays(starttime)
    startday <- as.integer(factor(startday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                       "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE))
    endday <- weekdays(endtime)
    endday <- as.integer(factor(endday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                   "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE))
    
    duration <- difftime(endtime, starttime, units="hours")
    leafletProxy('parkingmap') %>% clearMarkers()
    values$parkingtable = NULL
    values$parkingtable1 = NULL
    parkingviolation <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/parking_violation.csv")
    crimerate <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Crime_Rate/Felony.csv")
    if(input$price == 'Free'){
      parkingspots <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Free_Parking_Spots/Sample_new.csv")
      parkingspots <- parkingspots[parkingspots$Duration >= duration, ]
      
      parkingspots <- parkingspots[abs(parkingspots$Lat - clat1) <= 1, ]
      parkingspots <- parkingspots[abs(parkingspots$Lon - clng1) <= 1, ]
      
      if(nrow(parkingspots) > 0){
        rownames(parkingspots) <- 1:nrow(parkingspots)
        parkingspots_temp <- parkingspots[0,]
        for (i in 1:nrow(parkingspots)){
          if(parkingspots[i,'WE'] >= parkingspots[i,'WS']){
            dayrangeorder <- c(parkingspots[i,'WS']:parkingspots[i,'WE'])
          } else{
            dayrangeorder <- c(parkingspots[i,'WS']:7, 1:parkingspots[i,'WE'])
          }
          flag <- FALSE
          if((startday %in% dayrangeorder) & (endday %in% dayrangeorder)) {
            if(match(startday,dayrangeorder) <= match(endday,dayrangeorder)){
              flag <- TRUE
              if(startday == dayrangeorder[1] & starttime < parkingspots[i,'TS']){
                flag <- FALSE}
              if(endday == dayrangeorder[length(dayrangeorder)] & endtime > parkingspots[i, 'TE']) {
                flag <- FALSE}
              }
          }
          if(flag){
            temp_distance <- distance_calculation(parkingspots[i,"Lat"], parkingspots[i,"Lon"], clat1, clng1)
            if(temp_distance <= maxdist){
              parkingspots_temp[nrow(parkingspots_temp)+1, seq(8)] <- parkingspots[i,]
              unsafeindex <- nrow(parkingviolation[abs(parkingviolation$Lon - parkingspots[i,"Lon"]) <= 0.01 & 
                                                     abs(parkingviolation$Lat - parkingspots[i, "Lat"]) <= 0.01, ]) +
                nrow(crimerate[abs(crime$Longitude - parkingspots[i,"Lon"]) <= 0.01 & 
                                        abs(crime$Latitude - parkingspots[i,"Lat"]) <= 0.01, ])
              parkingspots_temp[nrow(parkingspots_temp), "Unsafe"] <- unsafeindex
              parkingspots_temp[nrow(parkingspots_temp), "Distance"] <- temp_distance
            }
          }
        }
        parkingspots  <- parkingspots_temp
      } else 
        return()
      
      if(nrow(parkingspots) > 0) {
        parkingspots <- parkingspots[order(parkingspots$Distance, parkingspots$Unsafe),]
        leafletProxy('parkingmap') %>%
        addMarkers(data = parkingspots[parkingspots$Unsafe <= 18, ], 
                   lng = ~ Lon, lat = ~ Lat,  icon=greenparkingLeafIcon,
                   popup = paste(parkingspots$Address, 
                                 parkingspots$Unsafe, sep=", dangerous index: "),
                   layerId = ~ Address) %>%
        addMarkers(data = parkingspots[(parkingspots$Unsafe <= 30) & (parkingspots$Unsafe > 18), ], 
                   lng = ~ Lon, lat = ~ Lat,  icon=yellowparkingLeafIcon,
                   popup = paste(parkingspots$Address, 
                                 parkingspots$Unsafe, sep=", dangerous index: "),
                   layerId = ~ Address) %>%
        addMarkers(data = parkingspots[parkingspots$Unsafe > 30, ], 
                   lng = ~ Lon, lat = ~ Lat,  icon=redparkingLeafIcon,
                   popup = paste(parkingspots$Address, 
                                 parkingspots$Unsafe, sep=", dangerous index: "),
                   layerId = ~ Address) }
              
      values$parkingtable <- parkingspots
    }
    else if(input$price == 'Paid'){
      maxprice <- input$pricerange[2]
      minprice <- input$pricerange[1]
      parkinglots <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/paid_parking.csv")
      parkinglots <- parkinglots[parkinglots$Rate <= maxprice & parkinglots$Rate >= minprice, ]
      
      if(nrow(parkinglots) > 0){
        parkinglots_temp <- parkinglots[0,]
        for (i in 1:nrow(parkinglots)){
          temp_distance <- distance_calculation(parkinglots[i,4], parkinglots[i,5], clat1, clng1)
          print(temp_distance)
          if(temp_distance <= maxdist){
            print(i)
            parkinglots_temp[nrow(parkinglots_temp)+1, seq(5)] <- parkinglots[i,]
            unsafeindex <- nrow(parkingviolation[abs(parkingviolation$Lon - parkinglots[i,5]) <= 0.01 & 
                                                   abs(parkingviolation$Lat - parkinglots[i, 4]) <= 0.01, ]) +
              nrow(crimerate[abs(crime$Longitude - parkinglots[i,5]) <= 0.01 & 
                               abs(crime$Latitude - parkinglots[i,4]) <= 0.01, ])
            parkinglots_temp[nrow(parkinglots_temp), "Unsafe"] <- unsafeindex
            parkinglots_temp[nrow(parkinglots_temp), "Distance"] <- temp_distance
            parkinglots_temp[nrow(parkinglots_temp), "Total_Fee"] <- duration * parkinglots_temp[nrow(parkinglots_temp), "Rate"]
          }
        }
      }
      
      if(nrow(parkinglots_temp) >0 ){
        parkinglots_temp <- parkinglots_temp[order(parkinglots_temp$Distance, parkinglots_temp$Unsafe),]
        leafletProxy('parkingmap') %>%
          addMarkers(data = parkinglots_temp[parkinglots_temp$Unsafe <= 18, ], 
                     lng = ~ Longitude, lat = ~ Latitude,  icon=greenparkingLeafIcon,
                     popup = paste(parkinglots$Address, 
                                   parkinglots$Unsafe, sep=", dangerous index: "),
                     layerId = ~ Address) %>%
          addMarkers(data = parkinglots_temp[(parkinglots_temp$Unsafe <= 30) & (parkinglots_temp$Unsafe > 18), ], 
                     lng = ~ Longitude, lat = ~ Latitude,  icon=yellowparkingLeafIcon,
                     popup = paste(parkinglots_temp$Address, 
                                   parkinglots_temp$Unsafe, sep=", dangerous index: "),
                     layerId = ~ Address) %>%
          addMarkers(data = parkinglots_temp[parkinglots_temp$Unsafe > 30, ], 
                     lng = ~ Longitude, lat = ~ Latitude,  icon=redparkingLeafIcon,
                     popup = paste(parkinglots_temp$Address, 
                                   parkinglots_temp$Unsafe, sep=", dangerous index: "),
                     layerId = ~ Address)
        
        values$parkingtable1 <- parkinglots_temp
      }
    }
  })
  
  
  ### Add markers on Facility map
  observeEvent(input$submit2, {
    if(is.null(input$facilitymap_click))
      return()
    facilitychosen <- input$facility
    click2<-input$facilitymap_click
    clat2 <- click2$lat
    clng2 <- click2$lng
    leafletProxy('facilitymap') %>% clearMarkers()
    if(exists('facilitydata_filtered')) {
      facilitydata_filtered <- facilitydata_filtered[0,]}
    if(facilitychosen == "Gas Station") {
      facilitydata <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Facilities/Gas Station in Manhattan.csv")
      facilitydata_filtered <- facilitydata[0,]
      for (i in 1:nrow(facilitydata)) {
        temp_distance <- distance_calculation(facilitydata[i,3], facilitydata[i,4], clat2, clng2)
        if(temp_distance <= input$distance2){
          facilitydata_filtered[nrow(facilitydata_filtered)+1, seq(4)] <- facilitydata[i,]
          facilitydata_filtered[nrow(facilitydata_filtered), 'Distance'] <- temp_distance
        }
      }
      if(nrow(facilitydata_filtered)!=0) {
        facilitydata_filtered <- facilitydata_filtered[order(facilitydata_filtered$Distance),]
        rownames(facilitydata_filtered) <- 1:nrow(facilitydata_filtered)
        leafletProxy('facilitymap') %>%
        addMarkers(data = facilitydata_filtered, lng = ~ Longitude, lat = ~ Latitude,  icon=gasstationLeafIcon,
                   popup = paste(facilitydata_filtered$Name, facilitydata_filtered$Address, sep=": "),
                   layerId = ~ Address)}}
    if(facilitychosen == "Garage") {
      facilitydata <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Facilities/garage_location.csv")
      facilitydata_filtered <- facilitydata[0,]
      for (i in 1:nrow(facilitydata)) {
        temp_distance <- distance_calculation(facilitydata[i,3], facilitydata[i,4], clat2, clng2)
        if(temp_distance <= input$distance2){
          facilitydata_filtered[nrow(facilitydata_filtered)+1, seq(4)] <- facilitydata[i,]
          facilitydata_filtered[nrow(facilitydata_filtered), 'Distance'] <- temp_distance
        }
      }
      if(nrow(facilitydata_filtered)!=0) {
        facilitydata_filtered <- facilitydata_filtered[order(facilitydata_filtered$Distance),]
        rownames(facilitydata_filtered) <- 1:nrow(facilitydata_filtered)
        leafletProxy('facilitymap') %>%
        addMarkers(data = facilitydata_filtered, lng = ~ Longitude, lat = ~ Latitude,  icon=garageLeafIcon,
                   popup = paste(facilitydata_filtered$Name, 
                                 facilitydata_filtered$Address, sep=": "),
                   layerId = ~ Address)}}
    if(facilitychosen == "Restroom") {
      facilitydata <- read.csv("/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Facilities/publictoilet(name,address,laditude, longditude).csv")
      facilitydata_filtered <- facilitydata[0,]
      for (i in 1:nrow(facilitydata)) {
        temp_distance <- distance_calculation(facilitydata[i,3], facilitydata[i,4], clat2, clng2)
        if(temp_distance <= input$distance2){
          facilitydata_filtered[nrow(facilitydata_filtered)+1, seq(4)] <- facilitydata[i,]
          facilitydata_filtered[nrow(facilitydata_filtered), 'Distance'] <- temp_distance
          }
        }
      if(nrow(facilitydata_filtered)!=0) {
        facilitydata_filtered <- facilitydata_filtered[order(facilitydata_filtered$Distance),]
        rownames(facilitydata_filtered) <- 1:nrow(facilitydata_filtered)
        leafletProxy('facilitymap') %>%
        addMarkers(data = facilitydata_filtered, lng = ~ Longitude, lat = ~ Latitude,  icon=restroomLeafIcon,
                   popup = paste(facilitydata_filtered$Name,  
                                 facilitydata_filtered$Address, sep=": "),
                   layerId = ~ Address) 
        }
      }
    values$facilitytable <- facilitydata_filtered
  })
  
  ### Highlight marker when 1) mouseover 2) corresponding row in table selected
  observe({
    leafletProxy("facilitymap") %>% clearGroup("overlays")
    event <- input$facilitymap_marker_mouseover
    if (is.null(event))
      return()
    isolate({
      leafletProxy("facilitymap") %>% addCircles(lng=event$lng,lat=event$lat-0.0007, radius=70, layerId = event$id,
                                                 fillColor="red", color="red", group="overlays")
    })
  })
  
  observe({
    leafletProxy("parkingmap") %>% clearGroup("overlays")
    event <- input$parkingmap_marker_mouseover
    if (is.null(event))
      return()
    isolate({
      leafletProxy("parkingmap") %>% addCircles(lng=event$lng,lat=event$lat-0.0005, radius=80, layerId = event$id,
                                                fillColor="blue", color="blue", group="overlays")
    })
  })
  
  observe({
    event <- input$outputtable2_rows_selected
    if (is.null(event))
      return()
    flon <- values$facilitytable[input$outputtable2_rows_selected, 4]
    flat <- values$facilitytable[input$outputtable2_rows_selected, 3]
    leafletProxy("facilitymap") %>% clearGroup("overlays")
    isolate({
      leafletProxy("facilitymap") %>% addCircles(lng=flon,lat=flat-0.0007, radius=70,
                                                 fillColor="red", color="red", group="overlays")
    })
  })
  
  observe({
    event <- input$outputtable_rows_selected
    if (is.null(event))
      return()
    flon <- values$parkingtable[input$outputtable_rows_selected,"Lon"] 
    flat <- values$parkingtable[input$outputtable_rows_selected,"Lat"]
    leafletProxy("parkingmap") %>% clearGroup("overlays")
    isolate({
      leafletProxy("parkingmap") %>% addCircles(lng=flon,lat=flat-0.0005, radius=80,
                                                fillColor="blue", color="blue", group="overlays")
    })
  })
  
  observe({
    event <- input$outputtable1_rows_selected
    if (is.null(event))
      return()
    flon <- values$parkingtable1[input$outputtable1_rows_selected,"Longitude"] 
    flat <- values$parkingtable1[input$outputtable1_rows_selected,"Latitude"]
    leafletProxy("parkingmap") %>% clearGroup("overlays")
    isolate({
      leafletProxy("parkingmap") %>% addCircles(lng=flon,lat=flat-0.0005, radius=80,
                                                fillColor="blue", color="blue", group="overlays")
    })
  })
  
  ### Other necessary functions
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
  
}

shinyApp(ui, server)
