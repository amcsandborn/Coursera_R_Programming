##########
# Lecture Week 4
##########

# Author: Avery Sandborn
# Date: February 19, 2016

##########

# Lesson 4: Geostatistics, IDW, and Field Data

# Task 4.2: Interpreting R Commands to produce an IDW interpolation of topo_dat.txt

# Create a list of all objects in the workspace and remove it (clears the workspace)
rm(list = ls(all = TRUE))

# Read the data table, there is a header
topo_datatable <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\topo_data.txt", header = TRUE)

# Load the spatial data library
library(sp)

# Set spatial coordinates of the data 
coordinates(topo_datatable) <- c("X", "Y")

# Sample point locations of the data, with sample size 3720, using a regular (systematically aligned) sampling method
topo_grid <- spsample(topo_datatable, "regular", n = 3720)

# Add a Gridded topology to the sampled points
gridded(topo_grid) <- TRUE

# Make the grid full and ordered (full matrix form)
fullgrid(topo_grid) <- TRUE

# Load the Geostatistical modeling library
library(gstat)

# Perform an Inverse distance weighted (IDW) interpolation on the topo data
  # weight nearer points over more distant points
  # Z~1 = simple and ordinary formula
  # newdata = grid with prediction locations
  # Inverse distance weighting power = 2
topo_idw <- idw(Z~1, topo_datatable, newdata = topo_grid, idp = 2.0)

# Convert the gridded idw into image format
im <- as.image.SpatialGridDataFrame(topo_idw)

# Load the map tools library
library(maptools)

# Convert from contour lines to spatial lines data frame object
topo_SLDF <- ContourLines2SLDF(contourLines(im))

# Create a list of 2 objects
topo_spl <- list("sp.lines", topo_SLDF)

# Plot the interpolation with the variable pred, and the contour lines on top
spplot(topo_idw, "var1.pred", sp.layout = topo_spl)

# Plot the image of the data with 20 colors
image(topo_idw, "var1.pred", col = terrain.colors(20))

# Plot the contour lines on top of the image
contour(topo_idw, "var1.pred", add = TRUE)

##########

# Task 4.3: Trend Surfaces

# Perform a kriging interpolation of the topo data: Develop a surface for optimum weights of the interpolation
  # z~1 = simple and ordinary formula
  # newdata = grid with prediction lcations
  # degree = 1 (linear)
topo_tsa_1 <- krige(Z~1, topo_datatable, newdata = topo_grid, degree = 1)

# Convert grided kriging into image format
im <- as.image.SpatialGridDataFrame(topo_tsa_1)

# Plot the image of the data with 5 colors
image(topo_tsa_1,"var1.pred",col=terrain.colors(5))

# Convert from contour lines to spatial lines data frame object
topo_tsa_line <-ContourLines2SLDF(contourLines(im))

# Plot the contour lines on top of the image
contour(topo_tsa_1,"var1.pred", add=TRUE)

# Perform a kriging interpolation of the topo data: Develop a surface for optimum weights of the interpolation
# z~1 = simple and ordinary formula
# newdata = grid with prediction lcations
# degree = 2 (quadratic!!)
topo_tsa_2 <- krige(Z~1, topo_datatable, newdata = topo_grid, degree = 2)

# Convert grided kriging into image format
im <- as.image.SpatialGridDataFrame(topo_tsa_2)

# Plot the image of the data with 5 colors
image(topo_tsa_2,"var1.pred",col=terrain.colors(5))

# Convert from contour lines to spatial lines data frame object
topo_tsa_line <-ContourLines2SLDF(contourLines(im))

# Plot the contour lines on top of the image
contour(topo_tsa_2,"var1.pred", add=TRUE)

# Read the data table, there is a header
topo_dat <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\topo_data.txt", header = TRUE)
x <- topo_dat$X
y <- topo_dat$Y
z <- topo_dat$Z
linear_surf <- lm(z ~ x + y)
linear_surf
quad_surf <- lm(z ~ x + y + I(x * x) + I(y * y) + I(x * y))
quad_surf

hist(linear_surf$residuals)

summary(linear_surf)

##########

# Task 4.4: Variography

# Plot the variogram cloud
topo_vg <- variogram(Z~1, data = topo_datatable, cloud = TRUE)
plot(topo_vg)

# See where outlier pairs of values occur on the map

# Select area in plot for analysis, and identify point pairs in the area
topoSV_edit <- plot(variogram(Z~1, topo_datatable, cloud = TRUE), digitize = TRUE)
plot(topoSV_edit, topo_datatable)

topo_vg <- variogram(Z~1, data = topo_datatable)
plot(topo_vg)

# Get quantities contained in the variogram
topo_vg

# Examine the variation stratified by direction
plot(variogram(Z~1, topo_datatable, alpha = c(0,45,90,135)))

##########

# Task 4.6: Spatial Inperpolation by Kriging

# List some variogram functions available in gstat
vgm()

# Spherical
vgm(1, "Sph", range = 390)

# Linear
vgm(0, "Lin")

# Occam's Razor
top_v_fit <- fit.variogram(topo_vg, vgm(2000, model = "Sph", range = 2.5, nugget = 0.0))

# Perform automatic Kriging on given dataset and generate experimental variogram
d <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\topo_data.txt", header = T)
attach(d)
library(sp)
library(automap)
coordinates(d) =~ X + Y
library(automap)
krige = autoKrige(Z~1, d)

krige$exp_var
krige$var_model
krige$sserr
plot(krige)

##########

# Interpolation By Kriging

topo_map_OK <- krige(Z~1, topo_datatable, topo_grid, top_v_fit)
topo_im <- as.image.SpatialGridDataFrame(topo_map_OK)
topo_SLDF <- ContourLines2SLDF(contourLines(topo_im))
topo_sp1 <- list("sp.lines", topo_SLDF)
image(topo_map_OK, "var1.pred", col = terrain.colors(20))
contour(topo_map_OK, "var1.pred", add = TRUE)

# Variance estimates can also be mapped
  # Used to suggest where additional data are/are not required
image(topo_map_OK, "var1.var", col = terrain.colors(20))
contour(topo_map_OK, "var1.var", add = TRUE)

