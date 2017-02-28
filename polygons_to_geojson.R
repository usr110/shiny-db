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


# test = '{
#   "type": "FeatureCollection",
# "features": [
# {
#   "type": "Feature",
#   "id":  " South Cambridgeshire 014B " ,
#   "geometry": {
#   "type": "Polygon",
#   "coordinates": [[
#   [-2.357941, -53.56315],  
#   [-0.6413269, 53.56315],  
#   [-0.6413269, 53.23563], 
#   [-2.357941, 53.23563],
#   [-2.357941, 53.23563]]]
# 
#   }
# }
#   ]
#   }'

test <- as.json(test)
 
leaflet() %>% addTiles() %>% addPolygons(data = geojson_sp(test), popup = dbdata$name)
