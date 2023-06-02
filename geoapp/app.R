library(maps)
library(leaflet)
library(shiny)
library(here)
library(tidyverse)

eq <- read_csv(here("2.5_month.csv"))

leaflet(eq) %>%
  addTiles() %>%
  addMarkers(lat = ~latitude, lng = ~longitude)

ui <- fluidPage(
  titlePanel("USGS Earthquakes"),
  leafletOutput("mymap"),
  fluidRow(column(2,
                  sliderInput("slider", "Select the magnitude", 0, 9, 2),
                  
                  radioButtons("radio", h3("Select the location source"),
                               choices = list("ak" = "ak", "ci" = "ci", "hv" = "hv", "ld" = "ld", "mb" = "mb", "nc" = "nc", "nm" = "nm", "nn" = "nn", "pr" = "pr", "pt" = "pt", "se" = "se", "us" = "us", "uu" = "uu", "uw" = "uw"), selected = "nc")
  ))
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(eq %>%
                filter(
                locationSource == input$radio,
                mag > input$slider)) %>%
      addTiles() %>%
      addMarkers(lat = ~latitude, lng = ~longitude)
  })
}

shinyApp(ui, server)
