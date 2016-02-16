##########
# Lecture Week 3
##########

# Author: Avery Sandborn
# Date: February 12, 2016

##########

library(spdep)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)

# Read in the polygon and plot it
lips <- readShapePoly("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\scotlip\\scotlip.shp", IDvar = "RECORD_ID", proj4string = CRS(as.character(NA)))
plot(lips)
lips

# Give thematic data as a data frame (basically attribute table)
as(lips, "data.frame")

# Plot cancer data as choropleth map
spplot(lips, "CANCER")

# Normalize chancer data by total male population
rate <- (lips$CANCER/lips$POP) * 1000
rate

# Join the data back into the spatial data frame to plot it
lips <- spCbind(lips, rate)
spplot(lips, "rate")

# Get raw number of cancer cases for each polygon
raw_nos <- lips$CANCER 
raw_nos
hist(raw_nos)

# Maps based on probabilities
  # Cancer expected
expected_nos <- lips$CEXP
expected_nos
sum(expected_nos)

# Plot exp cancer values
spplot(lips, "CEXP")

# Choynowski
  # Map the probability of getting a count more extreme than that observed under 
  # the assumptuion that the count in each area is Poisson with mean value mu. 
ch <- choynowski(lips$CANCER, lips$CEXP)
ch

# Spatial distribution of probabilities
  # pi < 0.05 >>> unusually high or low value
lips_extra <- spCbind(lips, ch$pmap)
spplot(lips_extra, "ch.pmap")

# Empirical Bayes
  # The higher the count, the more likely we are to have confidence in our 
  # mapped ratios
emp_bayes <- EBest(lips$CANCER, lips$POP)
emp_bayes

# Extract the parameter estimates
unlist(attr(emp_bayes, "parameters"))

# Use spCbind to Join emp_bayes back into the shapefile
lips_emp <- spCbind(lips, emp_bayes$estmm)

spplot(lips_emp, "emp_bayes.estmm")
       
##########

# Assignment Part 1: Produce a ratio and ???(chi-square map)

# Signed Chi Square Statistic
  # Squares of the differences between actual numbers in the zone and those 
  # expected if the variable in question were uniformly distributed over the 
  # entire area divided by this expected total.

# Simpler version
  # (Oi - Ei) / sqrt(Ei)

# Create a map fo the  sqrt(chi-square) stats for these same obs and exp counts 
# based simply on population years at risk measure

# http://personal.colby.edu/personal/m/mgimond/Spatial/Bailey_Gatrell_Auckland_child_mortality.html

lips <- readShapePoly("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\scotlip\\scotlip.shp", IDvar = "RECORD_ID", proj4string = CRS(as.character(NA)))
plot(lips)
lips

# Give thematic data as a data frame (basically attribute table)
as(lips, "data.frame")

# Chi Square Map
chi_data <- (lips$CANCER - lips$CEXP) / sqrt(lips$CEXP)
chi_data

# Join the data back into the spatial data frame to plot it
lips <- spCbind(lips, chi_data)

# Make map pretty
brks <- classIntervals(lips$chi_data, n = 11, style = "fixed", fixedBreaks = c(-11, -7, -5, -3, -1, 0, 1, 3, 5, 7, 11))
pal2 <- brewer.pal(11,"RdYlGn")
spplot(lips, "chi_data", at = brks$brks, col.regions = rev(pal2), scales = list(draw = TRUE), main = "Diversion from Expected Male Lip Cancer Rate", as.table = TRUE)

##########



##########

# Part 2: Global Spatial Autocorrelation

# Find shapefile relating to lattice irregular polygons

# Create a choropleth map of chosen variable

readShapePoly()
SpatialPolygonsDataFrame

# Design and justify an appropriate W matrix

# Compute and interprest Moran's I for global spatial autocorrelation

# Save your workspace 

##########

# Part 3: Local Spatial Autocorrelation

# Create a Moran Scatter Plot (MSP) and map of this as a LISA

















