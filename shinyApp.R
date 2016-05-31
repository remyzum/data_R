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

#~~~~~~ Big Global


# Import csv et Separation année mois jour heure 
coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

train <-
  read_csv(file="/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny/train.csv",
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

data3 <- top10[,c(2,8,9,4,12,11,10,13,14,15,7)]
names(data3)[1]<-"Name"
names(data3)[2]<-"Long"
names(data3)[3]<-"Lat"

setwd("/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny")
# avec write csv 1 
write.csv(data3, file ="data5.csv", row.names=FALSE)

#~~~~~ Small Global
data <- read_csv("data5.csv")
name <- sort(unique(data$Name))
dayofweek <- sort(unique(data$DayOfWeek))

ui <- (
  
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

server <- (function (input, output){
  
  #~~~~~~~~~~~~~ REACTIVES ~~~~~~~~~~~~
  
  date_seq <- reactive({seq(input$range[1], input$range[2], by = "day")}) 
  df0 <- reactive({    
    if (input$HoursOfDay == "All Day") {InterH <- data}
    else {InterH <- data[data$FourHours == input$HoursOfDay,]}
    return(InterH)}) 
  df1 <- reactive({filter(df0(), dayDate %in% date_seq())})
  
  #~~~~ Test avec les deux selectinputs
  datasetCat1 <- reactive({dfcat1<- df1()[df1()$Name == input$Category1,]}) 
  datasetCat2 <- reactive({dfcat2<- df1()[df1()$Name == input$Category2,]})
  dataset11 <- reactive({df11<- datasetCat1()[datasetCat1()$DayOfWeek == input$DayOfWeek,]})
  dataset22 <- reactive({df22<- datasetCat2()[datasetCat2()$DayOfWeek == input$DayOfWeek,]})
  datasetTEST <- reactive({rbind(dataset11(),dataset22())})
  #dataset111 <- reactive({ df111 <- ddply(dataset11(), .(Lat, Long), summarise, count = length(Address))})
  #dataset222 <- reactive({ df222 <- ddply(dataset22(), .(Lat, Long), summarise, count = length(Address))})
  datasetTTT <- reactive({ ddply(datasetTEST(), .(Lat, Long), summarise, count = length(Address))})
  
  #~~~~ Avec Un seul selectinput
  #dataset <- reactive({df0<- df1()[df1()$Name == input$Category,]}) 
  #dataset1 <- reactive({df2<- dataset()[dataset()$DayOfWeek == input$DayOfWeek,]}) 
  #dataset2 <- reactive({ df3 <- ddply(dataset1(), .(Lat, Long), summarise, count = length(Address))})
  
  
  
  #~~~~~~~~~~~~~ OUTPUT MAPS ~~~~~~~~~~~~    
  
  #~~~~~ Tab 1 basemap avec dots et renderleaflet et deux selects inputs dans UI
  output$map1 <- renderLeaflet({
    
    leaflet() %>% setView(lng = -122.431297, lat = 37.773972, zoom = 13) %>%
      addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = TRUE))  
    leaflet() %>% addTiles() %>%
      addCircleMarkers(lng = dataset11()$Long, lat = dataset11()$Lat, popup = paste(dataset11()$dayDate), radius=1, fillOpacity = 0.7, color = 'blue', fillColor = 'blue') %>%
      addCircleMarkers(lng = dataset22()$Long, lat = dataset22()$Lat, popup = paste(dataset22()$dayDate), radius=1, fillOpacity = 0.7, color = 'red', fillColor = 'red')
  })
  
  
  #~~~~~~~ Tab2 base map avec renderPlot (fixe pas de zoom)
  output$map2 <- renderPlot({
    
    SF <- get_map(location = "sanfrancisco", zoom = 12, source = "osm")
    ggmap(SF,extend = "panel") 
    #,base_layer = ggplot(aes(x = Long, y = Lat),data = data))
  })
  
  
  #~~~ Tab 2 - BasemapAvec RenderMap
  output$baseMap <- renderMap({
    baseMap <- Leaflet$new() 
    baseMap$setView(c(37.773972,-122.431297) ,12) 
    baseMap$addParams(height = 590, width = 600)
    baseMap
  })
  
  #~~~ Tab 2 - Heatmap 
  output$heatMap <- renderUI({
    
    ##~~~~~~ Creation de JSON avec 'paste0()'.
    
    
    j <- paste0("[",datasetTTT()[,"Lat"], ",", datasetTTT()[,"Long"], ",", datasetTTT()[,"count"], "]", collapse=",")
    j <- paste0("[",j,"]")
    
    
    ##~~~~~~ Fonctionne et layer ne se superposent pas
    tags$body(tags$script(HTML(sprintf("
                                       var addressPoints = %s
                                       if (typeof heat === typeof undefined) {
                                       heat = L.heatLayer(addressPoints, {radius: 12,blur: 6,maxZoom: 10,max: 6.0,
                                       gradient: {0.0: 'green',0.5: 'yellow',1.0: 'red' }}),
                                       heat.addTo(map)} 
                                       else {heat.setLatLngs(addressPoints)}", j ))))                                                  
  })
  })

shinyApp(ui, server)

#~~~~~ Heatmap fonctionne mais layers se superposent => invisible
#tags$body(tags$script(HTML(sprintf("
#                                   var addressPoints = %s
#                                  var heat = L.heatLayer(addressPoints).addTo(map)"
#                                    ,j1



#~~~~~ Tab 2 avec RenderLeaflet et ggmap
#    output$map2 <- renderLeaflet({
#      #leaflet() %>% setView(lng = -122.431297, lat = 37.773972, zoom = 13) %>%
#      #addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = TRUE)) %>% addTiles()

#         map.base <- ggmap(leaflet(), extend = "panel") + coord_cartesian() + coord_fixed(ratio = 1.5)

#    })


#~~~~ BaseMap avec RenderLeaflet
#output$basemap3  <- renderLeaflet({
#      leaflet(data=dataset1()) %>% setView(lng = -122.431297, lat = 37.773972, zoom = 13) %>%
#     addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = TRUE)) %>% addTiles() %>%
# addCircleMarkers(lng = ~ Long, lat = ~ Lat, popup = paste(dataset2()$dayDate), radius=1, fillOpacity = 0.7, fillColor = 'blue')

#})


#~~~~ HeatMap avec RenderUI
#output$heatMap <- renderUI({


# dataJSON<-reactive({toJSONArray2(dataset2(),json=F,names=F)})
#  dataJSON2<-reactive({rjson::toJSON(dataJSON())})
#  cat(rjson::toJSON(dataJSON2()[1:2]))
#  tags$body(tags$script(HTML(sprintf("<script>
#                                    var addressPoints = %s
#                                   var heat = L.heatLayer(addressPoints, {maxZoom: 14, radius: 20, blur: 25}).addTo(map)
#                                  </script>",dataJSON2()
#))))

#})


