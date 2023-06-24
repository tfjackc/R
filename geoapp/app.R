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

telemetry <- Telemetry$new()

url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"
earthquakes <- readOGR(url)
eqsf <- st_as_sf(earthquakes)
eqsf$time <- as.POSIXct(as.numeric(eqsf$time)/1000, origin = "1970-01-01", tz = "America/Los_Angeles")
eqsf$time_formatted <- format(eqsf$time, "%Y-%m-%d %I:%M:%S %p %Z")

eqsf_table <- eqsf %>%
  st_drop_geometry(eqsf) %>%
  select(mag, place, time_formatted)


ui <- fluidPage(
  titlePanel("USGS Earthquakes"),
  fluidRow(
    column(
      width = 2,
      sliderInput("slider", h4("Select the magnitude"), 2, 9, 2),
      selectInput(
        "dropdown",
        h4("Select the location source"),
        choices = c(
          "all", "ak", "ci", "hv", "ld", "mb", "nc", "nm", "nn", "pr",
          "pt", "se", "us", "uu", "uw"
        ),
        selected = "all"
      )
    ),
    column(width = 10,
    leafletOutput("eqMap"),
  )
  ),
  DT::dataTableOutput("timeTable"),
  use_telemetry(), # 2. Add necessary Javascript to Shiny
  numericInput("n", "n", 1),
  plotOutput('plot')
)

server <- function(input, output, session) {
  output$eqMap <- renderLeaflet({
    filteredEqsf <- eqsf
    
    if (input$dropdown != "all") {
      filteredEqsf <- filteredEqsf %>%
        filter(net == input$dropdown)
    }
    
    filteredEqsf <- filteredEqsf %>%
      filter(mag > input$slider)
    
    pal <- colorBin(
      palette = "Spectral",
      domain = filteredEqsf$mag,
      reverse = TRUE,
      bins = 5
    )
    
    leaflet(filteredEqsf) %>%
      addTiles() %>%
      setView(-117.841293, 46.195042, 3) %>%
      addCircleMarkers(
        fillColor = ~ pal(mag),
        radius = ~ filteredEqsf$mag * 2,
        stroke = FALSE,
        color = "black",
        fillOpacity = 0.6,
        popup = paste0(
          "<strong>Title:</strong> ", filteredEqsf$title,
          "<br><strong>Magnitude:</strong> ", filteredEqsf$mag,
          "<br><strong>MMI:</strong> ", filteredEqsf$mmi,
          "<br><strong>Sig:</strong> ", filteredEqsf$sig
        )
      )
  })
  output$timeTable <- DT::renderDataTable(eqsf_table, server = FALSE, options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))
  telemetry$start_session() # 3. Minimal setup to track events
  output$plot <- renderPlot({ hist(runif(input$slider)) })
                              
}

shinyApp(ui, server)
