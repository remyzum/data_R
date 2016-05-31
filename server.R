library(shiny)
library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)
library(magrittr)
library(leaflet)
library(rCharts)
library(rjson)
library(rMaps)
library(plyr)

shinyServer(function (input, output){
  
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



