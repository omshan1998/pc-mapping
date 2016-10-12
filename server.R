function(input, output, session) {

  #Sets the necessary filters read from the UI
  
  filteredData <- reactive({
    #newFilter <- function(mapFilter,hideCountry,countryFilter,crossFilterType,hideCrossCountry,countryCrossFilter,citation,year,gsRank,authors,university,publisher,keywordList){
      
    data <- newFilter(input$MapFilter,input$hideCountry,input$countryFilter,input$FilterType,input$HideCrossCountry,input$countryCrossFilter,input$citations,input$Year,input$GSRank,input$Authors,input$University,input$Publisher,input$KeywordList)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(50, 0, zoom = 2)
  })
  
  #HOW DOES ALL OF THIS WORK??   
  #create reactive colorVariable, which updates the color palette based on the type of map chosen.
  colorVariable <- reactive({
    filteredData()@data[["Count"]]
    #filteredData[["Count"]]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorBin("YlOrRd", colorVariable()) #you can change the color palette here.
  })
  
  #update map based on changed inputs figure out how this is working and see if its similar to the diagram!!! 
  observe({
    pal <- colorpal() #set the variable pal equal to the reactive variable colorpal.
    #Where the fourth option breaks!
    colorBy <- "Count"
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(stroke=FALSE, smoothFactor=0.2, 
                  color = ~pal(filteredData()@data[,colorBy]),
                  opacity = 1,
                  popup = ~paste("<strong>",colorBy,":</strong>",filteredData()@data[,colorBy], "<strong>Country:</strong>",NAME)) %>% 
      addLegend(title=colorBy, pal=colorpal(), values=filteredData()@data[,colorBy], position="bottomleft")
  })
}
