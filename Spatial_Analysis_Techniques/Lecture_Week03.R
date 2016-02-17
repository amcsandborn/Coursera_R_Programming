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

# Lecture Part 3.4

# Basically, plot connections/links of the zones

# Queens Case Contiguity: Area objects are neighbors if they share a boundary or single point
lips_nb <- poly2nb(lips, row.names = NULL, queen = TRUE)
summary(lips_nb)

# Create reference points inside each area to be taken as in some sense representative of each area as a matrix of coordinantes 
matrix <- coordinates(lips)
matrix

# Plot pattern of contiguities, create map of matrix and superimpose zone boundaries
plot(lips_nb, matrix)
plot(lips, add = TRUE)

# other exampes of approaches to neighbor zones
delauney_Scot <- tri2nb(matrix)
plot.nb(delauney_Scot, matrix)
plot(lips, add=TRUE)

SOI_Scot <- graph2nb(soi.graph(delauney_Scot, matrix))
plot.nb(SOI_Scot, matrix)
plot(lips, add=TRUE)

Gabriel_Scot <- graph2nb(gabrielneigh(matrix))
plot.nb(Gabriel_Scot, matrix)
plot(lips, add=TRUE)

relative_neigh_Scot <- graph2nb(relativeneigh(matrix))
plot.nb(relative_neigh_Scot, matrix)
plot(lips, add=TRUE)

# Alternative defined by the distances between centroids
  # based on k = 3 nearest neighbors
dist_3 <- knn2nb(knearneigh(matrix, k = 3))
plot.nb(dist_3, matrix)
plot(lips, add = TRUE)

# We now have 6 possible definitions of neighborhood:
  # lips, dist_3, delauney_Scot, SOI_Scot, Gabriel_Scot, relative_neigh_Scot

# Here's a variable...
rate

# Moran's I
  # assumes all zones have at least one neighbor

# Assign binary weights to all six neighbor lists
contig_listw <- nb2listw(lips_nb, style = "B", zero.policy = TRUE)
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
  # Significant positive global spatial autocorrelation

# Monte Carlo Procedure: location attributes are randomly assigned to the zones a specified number of times and a value for I calculated in each case
  # Enables the observed value to be ranked relative to these simulations 

# Monte carlo values for delaun_listw
set.seed = (4567)
moran.mc(rate, listw = delaun_listw, nsim = 99)
  # Resuts confirm what we have already seen

##########

# Lecture Part 3.5: Local Statistics and the Moran ScatterPlot

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
lips_extra <- spCbind(lips_extra, local_I)
names (lips_extra)

# Draw choropleth map of values of local moran's I that show areas unusually high or low relative to their neighbors
spplot(lips_extra, "local_I")






