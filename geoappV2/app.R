library(tidyverse)
library(maps)
library(leaflet)
library(shiny)
library(here)
library(sf)
library(rgdal)

# think about pulling in main tectonic boundaries and buffering and then finding earthquake density in certain global zones.

url <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_month.geojson"
earthquakes <- readOGR(url)
eqsf <- st_as_sf(earthquakes)

ui <- fluidPage(
  titlePanel("USGS Earthquakes"),
  leafletOutput("eqMap"),
  fluidRow(
    column(
      width = 4,
      sliderInput("slider", h4("Select the magnitude"), 2, 9, 2),
      
    ),
    column(
      width = 4,
      selectInput(
        "dropdown",
        h4("Select the location source"),
        choices = c("all", "ak", "ci", "hv", "ld", "mb", "nc", "nm", "nn", "pr", "pt", "se", "us", "uu", "uw"),
        selected = "all"
      )
    )
  )
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
        fillColor = ~pal(mag),
        radius = ~filteredEqsf$mag * 2,
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
}

shinyApp(ui, server)