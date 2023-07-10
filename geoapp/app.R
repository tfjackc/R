library(tidyverse)
library(maps)
library(leaflet)
library(shiny)
library(DT)
library(here)
library(sp)
library(sf)
library(rgdal)
library(lubridate)
library(shiny.telemetry)
library(leaflet.extras)
library(geojsonsf)
library(geojsonio)
library(dbscan)
library(factoextra)
library(mapboxapi)
library(RColorBrewer)
library(basemaps)
library(ggmap)


url_month <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"
url_week <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_week.geojson"
url_day <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson"
#color_list <- c("Reds", "Spectral", "Pastel1", "PuRd", "PuBuGn")
color_list = rownames(subset(brewer.pal.info, category %in% c("seq", "div")))

world <- map_data("world")
#WorldData <- map_data('world')
#wdf <- st_as_sf(WorldData, coords = c("long", "lat"), crs = 4326)
#print(st_crs(wdf))

ui <- htmlTemplate("template.html",
                   map = leafletOutput("eqMap", height="100%"),
                   timeTable = dataTableOutput("timeTable"),
                   dbplot = plotOutput("dbscan_plot"),
                   slider = sliderInput("slider", h4("Select the magnitude"), 2, 9, value=c(2, 8)),
                   #dropdown = selectInput("dropdown",
                   #                       h4("Select the location source"),
                   #                      choices = c(
                   #                       "all", "ak", "ci", "hv", "ld", "mb", "nc", "nm", "nn", "pr",
                   #                      "pt", "se", "us", "uu", "uw"
                   #                   ),
                   #                  selected = "all"),
                   dataSelect = selectInput("dataSelect", h4("Select GeoJSON Feed"),
                                            choices = c("1 Month", "1 Week", "1 Day"),
                                            selected = "1 Month"),
                   color_choice = selectInput("color_choice", "Symbology", color_list, selected = "RdBu"),
                   checkbox = checkboxInput("legend", "Show legend", TRUE)
)


server <- function(input, output, session) {
  
  output$eqMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter", options = tileOptions(noWrap = FALSE)) %>% # add CARTO tiles
      addTiles(
        urlTemplate = "http://{s}.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw",
        options = tileOptions(
          id = "mapbox/satellite-v9",  # Replace with your desired Mapbox style ID
          accessToken = "pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw"  # Replace with your Mapbox access token
        ),
        group = "Satellite"
      ) %>%
      addTiles(group = "OSM") %>%
      # addMapboxTiles(style_id = "satellite",
      #               style_url = 'http://{s}.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw',
      #              access_token = "pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGpxcW9hNWwwODVrM2ZtaXUwOWhzMjNjIn0.-Oqp3xopqBxOXvHhqC3qFw",
      #             username = "tfjackc",
      #            group = "Satellite") %>%
      setView(-18.525960, 26.846869, 3) %>%
      addLayersControl(overlayGroups = c("vectorData"), baseGroups = c("DarkMatter", "Satellite", "OSM")) %>%
      addDrawToolbar(editOptions = editToolbarOptions()) 
  })
  
  dataInput <- reactive({
    if (input$dataSelect != "1 Month" & input$dataSelect != "1 Week") {
      url_day
    } else if (input$dataSelect != "1 Month" & input$dataSelect != "1 Day") {
      url_week
    } else if (input$dataSelect != "1 Week" & input$dataSelect != "1 Day") {
      url_month
    }
  })
  
  filteredEqsf <- reactive({
    earthquakes <- read_sf(dataInput())
    eqsf <- st_as_sf(earthquakes)
    eqsf <- st_transform(eqsf, 4326)
    eqsf$time <- as.POSIXct(as.numeric(eqsf$time)/1000, origin = "1970-01-01", tz = "America/Los_Angeles")
    eqsf$time_formatted <- format(eqsf$time, "%Y-%m-%d %I:%M:%S %p %Z")
    eqsf_table <- eqsf %>%
      st_drop_geometry(eqsf) %>%
      select(mag, place, time_formatted)
    
    filteredData <- eqsf %>%
      filter(mag >= input$slider[1] & mag <= input$slider[2])
    
    pal <- colorBin(
      palette = input$color_choice,
      domain = filteredData$mag,
      reverse = TRUE,
      bins = 5
    )
    
    list(filteredData = filteredData, pal = pal, eqsf_table = eqsf_table, eqsf = eqsf)
  })
  
  
  observe({
    filteredData <- filteredEqsf()$filteredData
    pal <- filteredEqsf()$pal
    
    leafletProxy("eqMap", data = filteredData) %>%
      clearMarkers() %>%
      addCircleMarkers(
        fillColor = ~pal(mag),
        radius = ~(mag*2), 
        weight = 1,
        stroke = FALSE,
        color = "#777777",
        fillOpacity = 0.6,
        popup = paste0(
          "<strong>Title:</strong> ", filteredData$title,
          "<br><strong>Time:</strong> ", format(filteredData$time, "%Y-%m-%d %I:%M:%S %p %Z"),
          "<br><strong>Magnitude:</strong> ", filteredData$mag
        ),
        group = "vectorData"
      ) %>%
      clearControls() %>%
      addLegend("topleft", pal = pal, values = ~filteredData$mag,
                title = "Magnitude",
                opacity = 1)
  })
  
  observe({
    eqsf_table <- filteredEqsf()$eqsf_table
    output$timeTable <- DT::renderDataTable(eqsf_table, server = FALSE, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  
  # Start of Drawing
  observeEvent(input$eqMap_draw_start, {
    print("Start of drawing")
    print(input$leafmap_draw_start)
  })
  
  # Stop of Drawing
  observeEvent(input$eqMap_draw_stop, {
    print("Stopped drawing")
    print(input$leafmap_draw_stop)
  })
  
  # New Feature
  observeEvent(input$eqMap_draw_new_feature, {
    print("New Feature")
    print(input$eqMap_draw_new_feature)
  })
  
  # Edited Features
  observeEvent(input$eqMap_draw_edited_features, {
    print("Edited Features")
    print(input$eqMap_draw_edited_features)
  })
  
  # Deleted features
  observeEvent(input$eqMap_draw_deleted_features, {
    print("Deleted Features")
    print(input$eqMap_draw_deleted_features)
  })
  
  # We also listen for draw_all_features which is called anytime
  # features are created/edited/deleted from the map
  observeEvent(input$eqMap_draw_all_features, {
    
    eqsf <- filteredEqsf()$eqsf
    
    print("All Features")
    print(input$eqMap_draw_all_features)
    
    if (!is.null(input$eqMap_draw_all_features) && length(input$eqMap_draw_all_features$features) > 0) {
      
      
      numFeatures <- length(input$eqMap_draw_all_features$features)
      lng <- input$eqMap_draw_all_features$features[[numFeatures]]$geometry$coordinates[1]
      lat <- input$eqMap_draw_all_features$features[[numFeatures]]$geometry$coordinates[2]
      radius <- input$eqMap_draw_all_features$features[[numFeatures]]$properties$radius
      
      print(paste0("geom coordinates: ", lat, ", ", lng))
      
      if (!is.null(radius)) {
        print(paste("radius: ", round(radius, digits = 2), "m"))
        
        # Convert radius from meters to decimal degrees
        new_geom <- data.frame(lon = as.numeric(lng), lat = as.numeric(lat))
        new_geom <- st_as_sf(new_geom, coords = c("lon", "lat"), crs = 4326) #4979 change before deployment to 4326
        
        print("-------eqsf----------")
        print(st_crs(eqsf))
        print("-------new_geom------")
        print(st_crs(new_geom))
        
        
        circle_geom <- st_buffer(new_geom, radius)
        
        #bbox <- st_bbox(circle_geom)
        #xmin <- as.numeric(bbox["xmin"])
        #ymin <- as.numeric(bbox["ymin"])
        #xmax <- as.numeric(bbox["xmax"])
        #ymax <- as.numeric(bbox["ymax"])
        #list(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
        
        circle_pts <- st_intersection(eqsf, circle_geom)
        df <- st_as_sf(circle_pts)
        df_coords <- data.frame(st_coordinates(df))
        locs <- dplyr::select(df_coords,X,Y) 
        db <- dbscan::dbscan(locs,eps=0.45,minPts = 5)
        
        
        output$dbscan_plot <- renderPlot({
          
          
          #cluster_data_sf <- st_as_sf(locs, coords = c("X", "Y"))
          #View(cluster_data_sf)
          
          bbox <- st_bbox(circle_geom)
          ymin <- as.numeric(bbox['ymin'])
          ymax <- as.numeric(bbox['ymax'])
          xmax <- as.numeric(bbox['xmax'])
          xmin <- as.numeric(bbox['xmin'])
          
          cluster_data <- factoextra::fviz_cluster(db, locs, stand = FALSE, ellipse = TRUE, geom = "point")
          
          cluster_data + 
            geom_map(
              data = world, map = world,
              aes(long, lat, map_id = region)
            ) +
            coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
          
          
          
          #basemap <- basemap_ggplot(bbox, map_service = "osm", map_type = "streets")
          
          #plot(cluster_data)
          #plot(basemap)
          
          #bbox <- st_bbox(circle_geom)
          #world_sf <- st_as_sf(world_coordinates, coords = c("long", "lat"))
          
          #world_cropped <- st_crop(na.omit(wdf), xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
          #world_inter <- st_intersection(world, cluster_data_sf)
          
          #basemap_magick(bbox, map_service = "osm", map_type = "streets")
          #cluster_data + 
          #  basemap_ggplot(bbox, map_service = "osm", map_type = "streets")
          #basemap_ggplot(bbox, map_service = "osm", map_type = "streets") +
          
          
          
        })
        
        
        
      }
    }
  })
  
}


shinyApp(ui, server)