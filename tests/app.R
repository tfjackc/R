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



ui <- htmlTemplate("template.html",
                   map = leafletOutput("eqMap", height="100%"),
                   timeTable = dataTableOutput("timeTable"),
                  # dbplot = plotOutput("dbscan_plot"),
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
                                            selected = "1 Month")
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
      # addMapboxTiles(style_id = "satellite",
      #               style_url = 'http://{s}.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGhhd3VsZHAwbHV1M3RudGt0bWFhNHl0In0.5qDpeYjN5r-rBh-SYA9Qgw',
      #              access_token = "pk.eyJ1IjoidGZqYWNrYyIsImEiOiJjbGpxcW9hNWwwODVrM2ZtaXUwOWhzMjNjIn0.-Oqp3xopqBxOXvHhqC3qFw",
      #             username = "tfjackc",
      #            group = "Satellite") %>%
      setView(-18.525960, 26.846869, 3) %>%
      addLayersControl(overlayGroups = c("vectorData"), baseGroups = c("DarkMatter", "Satellite")) %>%
      addDrawToolbar(editOptions = editToolbarOptions())
  })
  
  
  url_month <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"
  url_week <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_week.geojson"
  url_day <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson"
  
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
    eqsf$time <- as.POSIXct(as.numeric(eqsf$time)/1000, origin = "1970-01-01", tz = "America/Los_Angeles")
    eqsf$time_formatted <- format(eqsf$time, "%Y-%m-%d %I:%M:%S %p %Z")
    eqsf_table <- eqsf %>%
      st_drop_geometry(eqsf) %>%
      select(mag, place, time_formatted)
    
    filteredData <- eqsf %>%
      filter(mag >= input$slider[1] & mag <= input$slider[2])
    
    pal <- colorBin(
      palette = "Spectral",
      domain = filteredData$mag,
      reverse = TRUE,
      bins = 5
    )
    
    list(filteredData = filteredData, pal = pal, eqsf_table = eqsf_table)
  })
  
    
  observe({
    filteredData <- filteredEqsf()$filteredData
    pal <- filteredEqsf()$pal
    
    leafletProxy("eqMap", data = filteredData) %>%
      clearMarkers() %>%
      addCircleMarkers(
        fillColor = ~pal(mag),
        radius = ~mag * 2,
        stroke = FALSE,
        color = "black",
        fillOpacity = 0.6,
        popup = paste0(
          "<strong>Title:</strong> ", filteredData$title,
          "<br><strong>Time:</strong> ", format(filteredData$time, "%Y-%m-%d %I:%M:%S %p %Z"),
          "<br><strong>Magnitude:</strong> ", filteredData$mag,
          "<br><strong>MMI:</strong> ", filteredData$mmi,
          "<br><strong>Sig:</strong> ", filteredData$sig
        ),
        group = "vectorData"
      )
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
  
  
}


shinyApp(ui, server)