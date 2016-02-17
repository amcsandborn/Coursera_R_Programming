##########
# Assignment Week 3
##########

# Author: Avery Sandborn
# Date: February 12, 2016

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

library(spdep)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)

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

# Assignment Part 2: Global Spatial Autocorrelation

library(spdep)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)

# Metadata: file:///C:/Users/sandav/Downloads/ohiolung/ohiolung_metadata.html

# Find shapefile relating to lattice irregular polygons
lungs <- readShapePoly("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\ohiolung\\ohlung.shp", IDvar = "RECORD_ID", proj4string = CRS(as.character(NA)))
plot(lungs)
lungs

# Give thematic data as a data frame (basically attribute table)
as(lungs, "data.frame")

# Create a choropleth map of chosen variable
spplot(lungs, "LMW68", scales = list(draw = TRUE), main = "Number of White Males in 1968 with Lung Cancer Cases", as.table = TRUE)

# Normalize cancer data by total white male population
rate <- (lungs$LMW68/lungs$POPMW68) * 100
rate

# Join the data back into the spatial data frame to plot it
lungs <- spCbind(lungs, rate)
spplot(lungs, "rate", scales = list(draw = TRUE), main = "Percent of White Males in 1968 with Lung Cancer Cases", as.table = TRUE)

lungs$LMW68
lungs$POPMW68

# Use spdep package to define and justify a W matrix

# Queens Case Contiguity: Area objects are neighbors if they share a boundary or single point
lungs_nb <- poly2nb(lungs, row.names = NULL, queen = TRUE)
summary(lungs_nb)

# Create reference points inside each area to be taken as in some sense representative of each area as a matrix of coordinantes 
matrix <- coordinates(lungs)
matrix

# Plot pattern of contiguities, create map of matrix and superimpose zone boundaries
plot(lungs_nb, matrix)
plot(lungs, add = TRUE)

# other exampes of approaches to neighbor zones
delauney_Scot <- tri2nb(matrix)
plot.nb(delauney_Scot, matrix)
plot(lungs, add=TRUE)

SOI_Scot <- graph2nb(soi.graph(delauney_Scot, matrix))
plot.nb(SOI_Scot, matrix)
plot(lungs, add=TRUE)

Gabriel_Scot <- graph2nb(gabrielneigh(matrix))
plot.nb(Gabriel_Scot, matrix)
plot(lungs, add=TRUE)

relative_neigh_Scot <- graph2nb(relativeneigh(matrix))
plot.nb(relative_neigh_Scot, matrix)
plot(lungs, add=TRUE)

# Alternative defined by the distances between centroids
# based on k = 3 nearest neighbors
dist_3 <- knn2nb(knearneigh(matrix, k = 3))
plot.nb(dist_3, matrix)
plot(lungs, add = TRUE)

# Moran's I
# assumes all zones have at least one neighbor

# Assign binary weights to all six neighbor lists
contig_listw <- nb2listw(lungs_nb, style = "B", zero.policy = TRUE)
dist_3_listw <- nb2listw(dist_3, style = "B")
SOI_listw <- nb2listw(SOI_Scot, style = "B", zero.policy = TRUE)
Gabriel_listw <- nb2listw(Gabriel_Scot, style = "B", zero.policy = TRUE)
rel_neigh_listw <-nb2listw(relative_neigh_Scot, style = "B", zero.policy = TRUE)
delaun_listw <- nb2listw(delauney_Scot, style = "B")

# Now compute global moran's I for the 6 different neighborhood lists
moran.test(rate, listw = contig_listw, zero.policy = TRUE)
moran.test(rate, listw = delaun_listw)
moran.test(rate, listw = dist_3_listw)
moran.test(rate, listw = SOI_listw)
moran.test(rate, listw = Gabriel_listw, zero.policy = TRUE)
moran.test(rate, listw = rel_neigh_listw, zero.policy = TRUE)

# Results show...
# Slight positive global spatial autocorrelation, but mostly no spatial autocorrelation

# Monte Carlo Procedure: location attributes are randomly assigned to the zones a specified number of times and a value for I calculated in each case
# Enables the observed value to be ranked relative to these simulations 

# Monte carlo values for contig_listw
set.seed = (4567)
moran.mc(rate, listw = contig_listw, nsim = 99)
# Resuts confirm what we have already seen

##########

# Assignment Part 3: Local Spatial Autocorrelation

# Create a Moran Scatter Plot (MSP) and map of this as a LISA

# Transform the Variable rate into z scores
mean_rate <- mean(rate)
mean_rate

sd_rate <- sd(rate)
sd_rate

zrate <- (rate - mean_rate) / sd_rate
zrate

# Compute and Plot the Moran Scatter Plot with these normalized variables 
  # Visual explanation of spatial autocorrelation 
  # Vertical Axis: spatial lag = mean value of its defined neighbors 
    # Mean of the standardized neighbors as defined in W matrix
  # Horizontal Axis: Original variable
    # gives standard scores for the observed values in each of the n areas
  # Upper right: positive autocorrelation: I'm high and neighbors are high
  # Lower Right: negative autocorrelation: I'm a high outlier and neighbors are low
  # Lower Left: positive autocorrelation: I'm low and my neighbors are low
  # Upper Left: negative autocorrelation: I'm a low outlier and neighbors are high
moran.plot(zrate, listw = delaun_listw)

# Compute zone values of local I, its expected values, zscore, and p value >> put into matrix
local_delaun <- localmoran(zrate, delaun_listw)
local_delaun

# Extract specific columns and use them in standard operations 
local_I <- local_delaun[,1]
local_I
hist(local_I)

# Bind them back to a data frame and draw maps
lungs <- spCbind(lungs, local_I)
names(lungs)

# Draw choropleth map of values of local moran's I that show areas unusually high or low relative to their neighbors
spplot(lungs, "local_I", scales = list(draw = TRUE), main = "Local Moran's I", as.table = TRUE)
