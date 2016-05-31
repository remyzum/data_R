library(shiny)
library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)

shinyUI(
  
  fluidPage(
    
    titlePanel("San Fransisco Crime Scene Map"),
    

    sidebarPanel(
      
      #~~~~~~ Avec deux select inputs ~~~~~
      selectInput("Category1", "Select a  first type of crime :", c(name), selected = "ASSAULT", selectize = FALSE),
      selectInput("Category2", "Select a second type of crime :", c("NONE (default)",name), selected = "NONE (default)", selectize = FALSE),
      
      #~~~~~~ Avec un selectizeinput et 3 categories max ~~~~~
      #selectizeInput("Category", "Choose a Type of Crime (max 3) :", c(name), multiple =TRUE, options = list(maxItems = 3)),
      
      sliderInput("range", label = "Select a Date Range :", min = as.Date(min(data$dayDate)), max = as.Date(max(data$dayDate)),#"2003-01-06",
                  value = c(as.Date("2009-01-04"),as.Date("2015-05-14")), round = FALSE,
                  animate = FALSE,
                  step = 365,
                  dragRange = FALSE,
                  timeFormat = "%F"),
      ##dateInput("inputId", " Select a Day:", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
      radioButtons("DayOfWeek", " Select a Day :", c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), selected = NULL, inline = FALSE, width = NULL),
      radioButtons("HoursOfDay", "Select a 4 hours Duration :", c("All Day","00H-04H","04H-08H","08H-12H","12H-16H","16H-20H","20H-00H"), selected = "16H-20H", inline = FALSE, width = NULL)),
    
    mainPanel(
     
       tabsetPanel(
         tabPanel("Dots Map", leafletOutput("map1", height = 590, width = 600)),
         #tabPanel("Heat Map with ggmap", plotOutput("map2", height = 590, width = 600)),
         tabPanel("Heat Map",
                  showOutput("baseMap", "leaflet"),
                  tags$style('.leaflet {height: 590px;}'),
                  tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
                  uiOutput("heatMap")
         )  
      )
                        
)))
  
