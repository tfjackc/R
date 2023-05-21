# load packages -----------------

library(tidyverse)
library(here)
library(sf)  
library(ggplot2)
library(leaflet)



# load my own data ---------------

aquifer <- read_csv(here("data", "Aquifers.csv"))

View(aquifer)
