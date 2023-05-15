# load packages -----------------

library(tidyverse)
library(here)
library(skimr)

# read in data -----------------

beaches <- read_csv(here("data", "sydneybeaches.csv"))

# exploring the data -----------

View(beaches)

dim(beaches)

str(beaches)

glimpse(beaches)

head(beaches)

tail(beaches)

summary(beaches)

skim(beaches)

# load my own data ---------------

bikeRide <- read_csv(here("data", "gpx_geoDF.csv"))

view(bikeRide)

dim(bikeRide)

str(bikeRide)

glimpse(bikeRide)

head(bikeRide)

tail(bikeRide)

summary(bikeRide)

skim(bikeRide)
