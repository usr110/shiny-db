library(leaflet)
library(DBI)
conn <- NULL

server <- function(input, output) {
  
  observe({
      all_cons <- dbListConnections(MySQL())
      if (length(all_cons) > 0)
        for(con in all_cons)
          +  dbDisconnect(con)
      
      print(paste(length(all_cons), " connections killed."))
  })
  
  
  
  # Initialize the leaflet map
  output$map <- renderLeaflet(
    leaflet() %>%
      addTiles(., 
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>%
      setView(lng = -1.5, lat = 53.4, zoom = 10) %>%
      #addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") %>%
      mapOptions(zoomToLimits = "first")
  )
  
  # Returns the map bounding box
  map_bb <- reactive({
    if (is.null(input$map_bounds)){ return (NULL)}
    x1 <- input$map_bounds$east
    x0 <- input$map_bounds$west
    y0 <- input$map_bounds$south
    y1 <- input$map_bounds$north
    #(x0, y0), (x0, y1), (x1, y1), (x1, y0), (x0, y0)
    #cat(" ----------- \n")
    #res <- paste( x0, y0, ", ",  "\n" , x0, y1, ", " ,  "\n", x1, y1, ", " ,  "\n",  x1, y0, ", " , "\n", x0, y0 )
    #cat(res, "\n")
    #cat(" ----------- \n")
    
    return(c(x0, x1, y0, y1))
  })
  
  observe({
    
    bbox <- map_bb()
    
    cat("length : ", length(bbox), "\n")
    if (is.null(bbox) || length(bbox) != 4)
      return()
    
    
    x0 <- bbox[1]
    x1 <- bbox[2]
    y0 <- bbox[3]
    y1 <- bbox[4]
    
    #(x0, y0), (x0, y1), (x1, y1), (x1, y0), (x0, y0)
    #cat(" ----------- \n")
    
    #cat(res, "\n")
    #cat(" ----------- \n")
    
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = db_cred$schema,
      host = db_cred$host,
      username = db_cred$username,
      password = db_cred$pwd
    )
    
    qry <- paste0("SELECT `lsoa`.`code`, `lsoa`.`name`, `lsoa`.`geometry`, AsText(`lsoa`.`bbox`) as bbox
                    FROM `lsoa`
                    WHERE MBRIntersects(`lsoa`.`bbox`, ST_GeomFromText('Polygon(
                    ", x0, " ", y0, ", ",
                    x0, " ", y1, ", ",
                    x1, " ", y1, ", ",
                    x1, " ", y0, ", ",
                    x0, " ", y0, " )'))")
    
    
    cat(qry, "\n")
    
    rs <- dbSendQuery(conn, qry)
    
    # rs <- dbSendQuery(conn, paste0("
    #                   SELECT `lsoa`.`code`, `lsoa`.`name`, `lsoa`.`geometry`, AsText(`lsoa`.`bbox`) as bbox
    #                   FROM `lsoa`
    #                   WHERE MBRIntersects(`lsoa`.`bbox`, ST_GeomFromText('Polygon(
    #                   ", x0, " ", y0, ", ",
    #                      x0, " ", y1, ", ",
    #                      x1, " ", y1, ", ",
    #                      x1, " ", y0, ", ",
    #                      x0, " ", y0, " )));"))
    
                      
    # cat(bbox[1], bbox[2], bbox[3], bbox[4], "\n")
    
    dbdata <- dbFetch(rs)
    dbDisconnect(conn)
    
    if (!is.null(dbdata) && nrow(dbdata) > 0){
      
      dat <- get_geojson_data(dbdata)
      isolate({
          addPolygons(leafletProxy("map"), 
                      data = dat,
                      fill = FALSE,
                      color = "black",
                      opacity = 0.7 ,
                      layerId = "highlighted")
    })
    
    }
      
  })
      
      get_geojson_data <- function(dbdata){
      
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
      
      test <- as.json(test)
      
      return(geojson_sp(test))
      }
      
      
      
}

ui <- fluidPage(
  
  leafletOutput("map", width="100%", height= 400)
)

shinyApp(ui = ui, server = server)
