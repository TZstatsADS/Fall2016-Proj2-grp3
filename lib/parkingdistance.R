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


plocation <- read.csv('locations.csv')

pd <- reactive({
  
  d1 <- input$distance
  suitable_dist <- vector()
  
  for (i in 1:nrow(plocation)) {
    d2 = distance_calculation(input$lat,input$lon,plocation$lat[i],plocation$lon[i])
    if(d2 <= d1) {
      suitable_dist = c(suitable_dist,plocation[i,1])
    }
  }
})
