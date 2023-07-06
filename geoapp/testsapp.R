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


#library(shinycssloaders)


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
                                            selected = "1 Month")
)


server <- function(input, output, session) {
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

  eqsf <- reactiveVal()

  observe({
    earthquakes <- readOGR(dataInput())
    eqsf_val <- st_as_sf(earthquakes)
    eqsf_val$time <- as.POSIXct(as.numeric(eqsf_val$time)/1000, origin = "1970-01-01", tz = "America/Los_Angeles")
    eqsf_val$time_formatted <- format(eqsf_val$time, "%Y-%m-%d %I:%M:%S %p %Z")
    eqsf_val_table <- eqsf_val %>%
      st_drop_geometry(eqsf_val) %>%
      select(mag, place, time_formatted)

    eqsf(eqsf_val)
    output$timeTable <- DT::renderDataTable(eqsf_val_table, server = FALSE, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"
      )
    ))
  })

  map <- reactiveValues(leafletMap = NULL)

  observe({
    filteredEqsf <- eqsf() %>%
      filter(mag >= input$slider[1] & mag <= input$slider[2])

    pal <- colorBin(
      palette = "Spectral",
      domain = filteredEqsf$mag,
      reverse = TRUE,
      bins = 5
    )

    if (!is.null(map$leafletMap)) {
      leafletProxy("eqMap") %>%
        clearGroup("vectorData") %>%
        addCircleMarkers(
          fillColor = ~pal(mag),
          radius = ~filteredEqsf$mag * 2,
          stroke = FALSE,
          color = "black",
          fillOpacity = 0.6,
          popup = paste0(
            "<strong>Title:</strong> ", filteredEqsf$title,
            "<br><strong>Time:</strong> ", filteredEqsf$time_formatted,
            "<br><strong>Magnitude:</strong> ", filteredEqsf$mag,
            "<br><strong>MMI:</strong> ", filteredEqsf$mmi,
            "<br><strong>Sig:</strong> ", filteredEqsf$sig
          ),
          group = "vectorData"
        )
      return(map$leafletMap)
    }
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
        new_geom <- st_as_sf(new_geom, coords = c("lon", "lat"), crs = 4979)

        circle_geom <- st_buffer(new_geom, radius)
        circle_pts <- st_intersection(eqsf(), circle_geom)
        df <- st_as_sf(circle_pts)
        df_coords <- data.frame(st_coordinates(df))
        locs <- dplyr::select(df_coords, X, Y)
        locs.scaled <- scale(locs, center = TRUE, scale = TRUE)

        print(locs.scaled)
        db <- dbscan::dbscan(locs.scaled, eps = 0.45, minPts = 5)

        output$dbscan_plot <- renderPlot({
          factoextra::fviz_cluster(db, locs.scaled, stand = FALSE, ellipse = TRUE, geom = "point")
        })

      }
    }
  })
  }


shinyApp(ui, server)
