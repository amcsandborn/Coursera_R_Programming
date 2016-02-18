##########
# Assignment Week 3 - Mid Week
##########

# Author: Avery Sandborn
# Date: February 18, 2016

##########

library(maptools)
library(sp)
library(GISTools)

# Read in shapefile
lips <- readShapePoly ("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\scotlip\\scotlip", IDvar = "RECORD_ID", proj4string = CRS(as.character(NA)))

# Compute rate per thousand
rate <- (lips$CANCER / lips$POP) * 1000
rate

# Create histogram of rate
hist(rate, col = "red")

# Join it back to rate
lips <- spCbind(lips, rate)

# Create a choropleth map of rate
choropleth(lips, lips$rate)

# Define classification scheme and shades
col_ramp <- auto.shading(lips$rate, n = 7, cutter = rangeCuts, cols = brewer.pal(7, "Greens"))

# Plot the map again
choropleth(lips, lips$rate, col_ramp)

# Chose location on map for Legend
locator(1)
choro.legend(27938.45, 1216361, col_ramp, fmt = "%5.3f", title = "Rate per Thousand", cex = 0.5)

# Add north arrow
locator(1)
north.arrow(428189.1, 1076127, 10000, cex = 0.6)

# Add Title
title("Rate per thousand person-years, 1975-1980")
