library(shiny)
library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)
library(magrittr)
library(leaflet)
library(rCharts)
library(rjson)
library(lubridate)
library(readr)
library(base)
library(stats)
library(ggmap)
library(devtools)


# Import csv et Separation année mois jour heure 
coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

train <-
  read_csv(file="/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/OneFile_SFCRIME/train.csv",
           col_types=coltypes)
train <-
  train %>% mutate(Year  = factor(year(Dates), levels=2003:2015), Month = factor(month(Dates), levels=1:12),
                   Month = month.abb[Month],
                   Day  = day(Dates),
                   Hour  = factor(hour(Dates), levels=0:23),
                   dayDate = as.POSIXct(round(Dates, units = "days")),
                   DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                          "Tuesday",
                                                          "Wednesday",
                                                          "Thursday",
                                                          "Friday",
                                                          "Saturday",
                                                          "Sunday"))
  )

# Classer par catégorie et compter les nombres de délits par catégorie
counts <- summarise(group_by(train, Category), Counts=length(Category))

# Compter le nombre de délit par catégorie du + au -
counts <- counts[order(-counts$Counts),]

# Ne garde que les 10 premières catégories et retire "Other Offenses" categorie avec (c(1,3:13))
top10 <- train[train$Category %in% counts$Category[c(1,3:12,17,20,31)],]
#category_10 <- unique(c(as.character(top10$Category))) ## Categorie de crimes

# Ajouter colonne avec intervalle de temps
top10$FourHours <- top10$Hour
levels(top10$FourHours) <- c(levels(top10$FourHours), "00H-04H")
top10[top10$Hour %in% c("0", "1", "2", "3"), "FourHours"] <- "00H-04H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "04H-08H")
top10[top10$Hour %in% c("4", "5", "6", "7"), "FourHours"] <- "04H-08H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "08H-12H")
top10[top10$Hour %in% c("8", "9", "10", "11"), "FourHours"] <- "08H-12H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "12H-16H")
top10[top10$Hour %in% c("12", "13", "14", "15"), "FourHours"] <- "12H-16H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "16H-20H")
top10[top10$Hour %in% c("16", "17", "18", "19"), "FourHours"] <- "16H-20H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "20H-00H")
top10[top10$Hour %in% c("20", "21", "22", "23"), "FourHours"] <- "20H-00H"

data3 <- top10[,c(2,8,9,4,12,11,10,13,14,15)]
names(data3)[1]<-"Name"
names(data3)[2]<-"Long"
names(data3)[3]<-"Lat"

setwd("/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny")
# avec write csv 1 
write.csv(data3, file ="data4.csv", row.names=FALSE)

data <- read_csv("data4.csv")
name <- sort(unique(data$Name))
dayofweek <- sort(unique(data$DayOfWeek))

ui <- (
  
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

server <- (function (input, output){
  
  #~~~~~~Text~~~~~~~~~
  #output$range <- renderText(input$range)
  #output$add <- renderText({paste("Category name:", input$Category)})
  
  
  output$map <- renderLeaflet({
    
    # By Range of Date 
    date_seq <- seq(input$range[1], input$range[2], by = "day")
    
    df0 <- reactive({    
      if (input$HoursOfDay == "All Day") {InterH <- data}
      else {InterH <- data[data$FourHours == input$HoursOfDay,]}
      return(InterH)})
    
    df1 <- reactive({filter(df0(), dayDate %in% date_seq)})
    dataset <- reactive({df<- df1()[df1()$Name == input$Category,]}) 
    dataset1 <- reactive({df2<- dataset()[dataset()$DayOfWeek == input$DayOfWeek,]}) 
    
    #~~~~~~Map~~~~~~~~~~~      
    leaflet() %>% setView(lng = -122.431297, lat = 37.773972, zoom = 13) %>%
      addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = TRUE))  
    leaflet(data=dataset1()) %>% addTiles() %>%
      addCircleMarkers(lng = ~ Long, lat = ~ Lat, popup = paste(dataset1()$dayDate), radius=1, fillOpacity = 0.7, fillColor = 'blue')
    ##leaflet(data=df[1:2000,]) %>% addTiles() %>%
    ##addMarkers(lng = ~ Long, lat = ~ Lat, popup = paste(df$dayDate)) 
    
  })
  
})



shinyApp(ui, server)
