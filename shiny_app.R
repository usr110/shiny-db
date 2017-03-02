library(leaflet)
library(geojsonio)
library(DBI)
library(RMySQL)


conn <- NULL
dbdata <- NULL

db_cred <- read.csv("db-info.csv", header = T, stringsAsFactors = F)

server <- function(input, output) {
  
  # Initialize the leaflet map
  output$map <- renderLeaflet(
    leaflet() %>%
      addTiles(., 
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options = tileOptions(minZoom = 7,
                                     maxZoom = 18, 
                                     reuseTiles = T)) %>%
      setView(lng = -1.5, lat = 53.4, zoom = 8) %>%
      mapOptions(zoomToLimits = "first") #%>%
      )
  
  
  observe({
      all_cons <- dbListConnections(MySQL())
      if (length(all_cons) > 0)
        for(con in all_cons)
          +  dbDisconnect(con)
      
      print(paste(length(all_cons), " connections killed."))
  })
  
  observe({
    
    if (is.null(input$map_bounds)){ return (NULL)}
    
    x1 <- input$map_bounds$east
    x0 <- input$map_bounds$west
    y0 <- input$map_bounds$south
    y1 <- input$map_bounds$north
    
    table_name <- "county"
    
    cat("Zoom is ", input$map_zoom, "\n")
    if(input$map_zoom > 9 && input$map_zoom < 14)
       table_name <- "msoa"
    if(input$map_zoom >= 14)
      table_name <- "lsoa"
    
    qry <- paste0("SELECT `name`, `geometry`, AsText(`bbox`) as bbox
                      FROM ", table_name , " 
                      WHERE MBRIntersects(`bbox`, ST_GeomFromText('Polygon((
                      ", x0, " ", y0, ", ",
                  x0, " ", y1, ", ",
                  x1, " ", y1, ", ",
                  x1, " ", y0, ", ",
                  x0, " ", y0, " ))'));")
    
    cat("Zoom is ", input$map_zoom, "\n")#, qry, "\n")
    
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = db_cred$schema,
      host = db_cred$host,
      username = db_cred$username,
      password = db_cred$pwd
    )
    
    rs <- dbSendQuery(conn, qry)
    
    dbdata <- dbFetch(rs)

    dbDisconnect(conn)

    if (!is.null(dbdata) && nrow(dbdata) > 0){
      
      clearGroup(leafletProxy("map"), c("poly"))
      
      local_dat <- get_geojson_data(dbdata)
      
      cat("adding polygons\n")

      leafletProxy("map")  %>% addPolygons(data = local_dat,
                                           popup = local_dat$name,
                                           group = "poly",
                                           color = "black",
                                           opacity = 0.7,
                                           layerId = paste0(local_dat$name, '-', "zones"))
      
    }
  })
  
  # Returns the map bounding box
  map_bb <- reactive({
    #input$map_zoom
    
    if (is.null(input$map_bounds)){ return (NULL)}
    # x1 <- isolate(input$map_bounds$east)
    # x0 <- isolate(input$map_bounds$west)
    # y0 <- isolate(input$map_bounds$south)
    # y1 <- isolate(input$map_bounds$north)
    
    x1 <- input$map_bounds$east
    x0 <- input$map_bounds$west
    y0 <- input$map_bounds$south
    y1 <- input$map_bounds$north
    
    return(c(x0, x1, y0, y1))
  })
  

  get_geojson_data <- function(raw_data){
    
    test = ""
    for (i in 1:nrow(raw_data)){
      
      if (i == 1){
        test = '
          {
          "type": "FeatureCollection",
          "features": [  '
      }
      
      test = paste0(test, '{
                      "type": "Feature",
                      "name": ',  '"' ,  raw_data$name[i],'"' , ',
                      "geometry": {
                      "type": "Polygon",
                      "coordinates": ', raw_data$geometry[i], '
                       }
                      },')
      
      # remove the last comma
      if (i == nrow(raw_data)){
        test = substr(test, 1, nchar(test) - 1)
        test = paste0(test, ' ]}')
      }
      
    }
    sink(tempfile())
    test <- as.json(test)
    sink()
    return(geojson_sp(test))
  }
  
}

ui <- fluidPage(
  
  leafletOutput("map", width="100%", height= 800)
)

shinyApp(ui = ui, server = server)
