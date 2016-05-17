library(shiny)
library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)
library(magrittr)
library(leaflet)
library(rCharts)
library(rjson)

shinyServer(function (input, output){

    #~~~~~~Text~~~~~~~~~
    #output$range <- renderText(input$range)
    #output$add <- renderText({paste("Category name:", input$Category)})
  

    output$map <- renderLeaflet({

      # By Range of Date 
      date_seq <- seq(input$range[1], input$range[2], by = "day")
      
      #df1 <- subset(data, as.Date(data$dayDate) >= as.Date(input$range[1]) | as.Date(data$dayDate) < as.Date(input$range[2])) 
      #sliderValue <- reactive({df<- data[data$Name == input$Category,]})
      # By day of week
      #dayofweek <- subset(data, data$DayOfWeek == input$DayOfWeek)
      # By 4-Hours interval
      #hoursinterval <- subset(data, data$FourHours == input$HoursOfDay)
      #HoursOfDays <- subset(data, data$Hour ==)
      ##df1 <- data[data$dayDate == input$range,]
      
    #~~~~~~Reactive~~~~~~~       
      #df0 <- reactive({data[data$FourHours == input$HoursOfDay,]})
      
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

