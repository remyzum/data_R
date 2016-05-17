library(shiny)
library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)

shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(
    
    titlePanel("San Fransisco Crime Scene Map"),
    
    sidebarPanel(
      selectInput("Category", "Choose a Type of Crime :", c(name), selected = "ASSAULT", selectize = FALSE),
      sliderInput("range", label = "Choose a Date Range :", min = as.Date(min(data$dayDate)), max = as.Date(max(data$dayDate)),#"2003-01-06",
                  value = c(as.Date("2015-01-01"),as.Date("2015-05-14")), round = FALSE,
                  animate = FALSE,
                  step = 365,
                  dragRange = FALSE,
                  timeFormat = "%F"),
      ##dateInput("inputId", " Choose a Day:", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
      radioButtons("DayOfWeek", " Choose a Day :", c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), selected = NULL, inline = FALSE, width = NULL),
      radioButtons("HoursOfDay", "Choose a 4 hours Duration :", c("All Day","00H-04H","04H-08H","08H-12H","12H-16H","16H-20H","20H-00H"), selected = NULL, inline = FALSE, width = NULL)),
    
    mainPanel(
      #uiOutput("add"),
      #uiOutput("range"),
      leafletOutput("map", height = 630, width = 600)
      #mapOutput("map")
      )
                        
))
  
#######  SHINY

# Set working directory
#getwd()
#setwd("/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny")

#shinyUI(fluidPage(
  
#  titlePanel(title = "San Fransisco Crime"),
#  sidebarLayout(position = "right",
#    sidebarPanel(h3("This is side bar panel"), h4("widget4"), h5("widget5")),
#    mainPanel( 
#        plotOutput(('map'),width = "100%"))
#sliderInput("range",
#           label = "Years to display:",
#            min = 2003,
#            max = 2015,
#            value = c(2003, 2015), 
#            animate = TRUE,
#            step = 1,
##            format = "0000")),
