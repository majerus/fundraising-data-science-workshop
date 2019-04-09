# add plotly graph

# SERVER ----
server <- function(input, output) {
  
  # generate random data for app
  source("random_data_generator_dynamic.R")
  
  # create model
  model_2 <- lm(af18 ~ af14 + af15 + af17 + cap14 + cap16 + cap17 + cap18, df)
  
  # predict fy18 annual gifts
  df$predicted_af18 <- predict(model_2, df) 
  
  # change negative predicted gifts to $0
  df$predicted_af18 <- ifelse(df$predicted_af18 < 0, 0, df$predicted_af18)
  
  # create leaflet base map  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -33.65, 
              lat = 35.0285, 
              zoom = 2
      )
    
  }) # end of base map 
  
  # add markers to base map  
  observe({
    
    leafletProxy("map") %>%
      clearMarkerClusters() %>%
      addCircleMarkers(lng = df$longitude, 
                       lat = df$latitude, 
                       label = df$name, 
                       radius = 5, 
                       stroke = FALSE, 
                       fillOpacity = 0.5, 
                       clusterOptions = markerClusterOptions()
      )
    
  }) # end of adding markers
  
  # Filter data based on bounds of map
  map_df_in_bounds <- reactive({
    
    if (is.null(input$map_bounds))
      return(df[FALSE,])
    
    # get boundaries of visible map
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    # filter data to visible observations 
    df %>% 
      filter(latitude >= latRng[1]  & 
               latitude <= latRng[2]  &
               longitude >= lngRng[1] & 
               longitude <= lngRng[2]
      ) 
    
  }) # end of map filter
  
  
  # create data table based on data filtered by inputs and map
  output$dt <- DT::renderDataTable({
    
    map_df_in_bounds() %>%
      select(name, 
             officer, 
             status, 
             city, 
             state, 
             af18, 
             predicted_af18
      ) %>%
      rename(predicted = predicted_af18) %>% 
      arrange(desc(af18)) %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    extensions = c('Responsive', 
                                   'Buttons')
      ) %>% 
      formatCurrency(c('af18', 
                       'predicted'), 
                     digits = 0)  
    
  }) # end of data table
  
  # create graph of actual and predict annual gifts using filtered data
  output$plot <- renderPlotly({
    
    plot_ly(data = map_df_in_bounds(), 
            x = ~af18, 
            y = ~predicted_af18)
    
  }) # end of graph code
  
}
