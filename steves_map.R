library(leaflet)

df %>% 
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, popup = ~name,
                   clusterOptions = markerClusterOptions())
