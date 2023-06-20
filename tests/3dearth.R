library(threejs)

coordinates <- st_coordinates(eqsf$geometry)
x <- coordinates[, 1]
y <- coordinates[, 2]
z <- coordinates[, 3]

# Plot the data on the globe
globejs(lat = y,
        long = x,
        pointsize = 0.5,
        atmosphere = TRUE)