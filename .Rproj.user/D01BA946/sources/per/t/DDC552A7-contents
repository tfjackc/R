# load packages -----------------

library(tidyverse)
library(here)
library(sf)  
library(ggplot2)
library(leaflet)



# load my own data ---------------

splitboard_data <- read_csv(here("data", "gpx_geoDF.csv"))

# View(splitboard_data)

# create a ggplot
ggplot(splitboard_data, aes(x = longitude, y = latitude, color = elevation)) +
  geom_point()

# Create a leaflet object.
m <- leaflet() %>%
  addTiles()

# or use the data in the leaflet object.
m <- m %>%
  addMarkers(data = splitboard_data, lat = ~latitude, lng = ~longitude)

# Display the leaflet object.
m
