##########
# Lecture Week 1
##########

# Author: Avery Sandborn
# Date: January 29, 2016

##########

# Helpful Websites:
  # http://rspatial.r-forge.r-project.org/gallery/

##########

# Load the data
library(spatstat)
data(redwood)
summary(redwood)

# Plot the data in a separate window
plot(redwood)

# Create a pixel image object
z <- density(redwood, 0.1)
plot(z)
summary(z)

# Create a pixel image with the points
plot(redwood, add=TRUE)

t <- rotate(redwood)
plot(t)
s <- shift(redwood)
plot(s)

# Generate a display of KDE, point pattern and proximity polygons
plot(z)
d <- dirichlet(redwood)
plot(d, add=T) # NB upper case for TRUE shortened to T

# Add the points
plot(redwood, add=T)

d <- delaunay(redwood) # NB the spelling used
plot(d)

nna <- clarkevans(redwood) # this is the standard 'nearest neighbor statistic for these data with an edge correction due to Donnelly
nna

#######################

library(spatstat)

# Read in text file
snow <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\SNOW_for_R.txt", header = TRUE)

# Attach it so that we can read individual (x,y) coordinates
attach(snow)
summary(X)
summary(Y)
nrow(snow)

# Create PPP object for use in spatstat
xlim <- c(8.0, 18.0)
ylim <- c(6.0, 17.0)
snow_2 <- ppp(X, Y, xlim, ylim)
plot(snow_2)

z2 <- density(snow_2, 0.1)
plot(z2, add = T)

data(snow_2)
summary(snow)

# Plot the data in a separate window
plot(snow)

# Create a pixel image object
z2 <- density(snow_2, 0.4)
plot(z2)
summary(z2)

# Create a pixel image with the points
plot(snow_2, add=TRUE)

setwd("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Results")
save(snow_2, file="snow_in_ppp")

################################

library(sp)
getClass("Spatial")

library(maps)
library(maptools)

sids_2 <- readShapePoly("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\sids2\\sids2", IDvar="FIPSNO", proj4string=CRS(as.character(NA)))
plot(sids_2)
plot(sids_2, border = "red", axes = TRUE)

# List attribute data in the data frame
sids_2
as(sids_2, "data.frame")

# Choropleth Maps
library(sp)
spplot(sids_2, "SID74")
spplot(sids_2, "SID79")

spplot(sids_2, "NWBIR79")

# Nicer Choropleth Map (title, axes, botom legend)
spplot(sids_2, c("NWBIR79"), names.attr = c("1979"), colorkey=list(space="bottom"), scales = list(draw = TRUE), main = "Total Non-White Births in North Carolina, 1979", as.table = TRUE)

##############################

library(maptools)
library(lattice)
library(sp)

data(meuse)
meuse

coordinates(meuse) <- c("x", "y")
spplot(meuse, "zinc")

# graduated symbol map?
bubble(meuse, "zinc")

topo_df <- read.table("N:/USERS/Avery/Spatial_Analysis_Techniques_in_R/topo_data.txt", header = TRUE)
topo_df

# extract the x y values using cbind and put them into topo_mat
topo_mat <- cbind(topo_df$X, topo_df$Y)
topo_mat

cord_ref <- CRS(as.character(NA))
topo_spdf <- SpatialPointsDataFrame(topo_mat, topo_df, proj4string = cord_ref)

spplot(topo_spdf, "Z")
bubble(topo_spdf, "Z")

#############################

# Import data into R and produce located symbol plot (color points and bubble)

library(maptools)
library(lattice)
library(sp)

simple_df <- read.table("N:/USERS/Avery/Spatial_Analysis_Techniques_in_R/Data/Simple_LA_OZ.txt", header = TRUE)
simple_df

# extract the x y values using cbind and put them into topo_mat
simple_mat <- cbind(simple_df$X, simple_df$Y)
simple_mat

cord_ref <- CRS(as.character(NA))
simple_spdf <- SpatialPointsDataFrame(simple_mat, simple_df, proj4string = cord_ref)

spplot(simple_spdf, "Z", scales = list(draw = TRUE), main = "Ozone Concentration in LA", as.table = TRUE)
bubble(simple_spdf, "Z", scales = list(draw = TRUE), main = "Ozone Concentration in LA", as.table = TRUE)

#################################

# Geocoding Locations

library(ggmap)

locations <- c("Maidwell, England", "Vienna, Austria")
locations

geocoded_locations <- cbind(locations, geocode(locations))
geocoded_locations

# More ways of geocoding locations

library(XML)
library(rgdal)
library(dismo)

place <- geocode("Maidwell, Northamptonshire, UK")
place

# Drawing Pin Maps with Google Earth
library(maptools)
library(rgdal)
library(sp)

cycle <- read.csv("N:/USERS/Avery/Spatial_Analysis_Techniques_in_R/Data/London_cycle_hire_locs.csv", header = T)
head(cycle)

plot(cycle$X, cycle$Y)

# Create a spatial points data frame object and add CRS projection
coordinates(cycle) <- c("X", "Y")
BNG <- CRS("+init=epsg:27700")
proj4string(cycle) <- BNG

# reproject points to wgs84 (used by google)
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
cycle_wgs84 <- spTransform(cycle, CRS = p4s)

# export data to KML
writeOGR(cycle_wgs84, dsn = "N:/USERS/Avery/Spatial_Analysis_Techniques_in_R/Data/London_cycle_docks.kml", layer = "cycle_wgs84", driver = "KML", dataset_options = c("NameFIELD = name"))

################

# grid which locations are located... 
BNG <- CRS("+init=epsg:27700")

# But we need to transform data into WGS84
proj4string(cycle) <- BNG
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
cycle_wgs84 <- spTransform(cycle, CRS=p4s)
