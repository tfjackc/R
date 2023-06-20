library(sf)
library(threejs)
coordinates <- st_coordinates(eqsf$geometry)
x <- coordinates[, 1]
y <- coordinates[, 2]
z <- coordinates[, 3]
scatterplot3js(x, y, z, color=rainbow(length(z)))

library(rgl)
plot3d(x=x, y=y, z=z)