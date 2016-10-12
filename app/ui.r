library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinyjs)
library(ggmap)

shinyUI(dashboardPage(
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
))