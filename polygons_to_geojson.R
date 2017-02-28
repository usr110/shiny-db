library(geojsonio)

test = ""
for (i in 1:nrow(dbdata)){
  
  if (i == 1){
    
    test = '
        {
          "type": "FeatureCollection",
          "features": [  '
  }
  
  test = paste0(test, '
    
  {
  "type": "Feature",
  "id": ',  '"' ,  dbdata$name[i],'"' , ',
  "geometry": {
  "type": "Polygon",
  "coordinates": ',  
    dbdata$geometry[i], '
  }
  },')
  
  # remove the last comma
  if (i == nrow(dbdata)){
    test = substr(test, 1, nchar(test) - 1)
    test = paste0(test, ' ]}') 
  }

}

leaflet() %>% addTiles() %>% addPolygons(data = geojson_sp(as.json(test)), popup = dbdata$name)