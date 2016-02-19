##########
# Assignment Week 4
##########

# Author: Avery Sandborn
# Date: February 19, 2016

##########

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

# Task 4.2 Fitting Trend Surfaces in R

# Fit linear and quadratic polynomials to LA ozone data file

library(sp)
library(gstat)
library(maptools)

# Read the data table, there is a header
LA_datatable <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\Simple_LA_OZ.txt", header = TRUE)

# Set spatial coordinates of the data 
coordinates(LA_datatable) <- c("X", "Y")

# Sample point locations of the data, with sample size 3720, using a regular (systematically aligned) sampling method
LA_grid <- spsample(LA_datatable, "regular", n = 3720)

# Add a Gridded topology to the sampled points
gridded(LA_grid) <- TRUE

# Make the grid full and ordered (full matrix form)
fullgrid(LA_grid) <- TRUE

# Perform an Inverse distance weighted (IDW) interpolation on the LA data
  # weight nearer points over more distant points
  # Z~1 = simple and ordinary formula
  # newdata = grid with prediction locations
  # Inverse distance weighting power = 2
LA_idw <- idw(Z~1, LA_datatable, newdata = LA_grid, idp = 2.0)

# Convert the gridded idw into image format
im <- as.image.SpatialGridDataFrame(LA_idw)

# Convert from contour lines to spatial lines data frame object
LA_SLDF <- ContourLines2SLDF(contourLines(im))

# Create a list of 2 objects
LA_spl <- list("sp.lines", LA_SLDF)

# Plot the interpolation with the variable pred, and the contour lines on top
spplot(LA_idw, "var1.pred", sp.layout = LA_spl)

# Plot the image of the data with 20 colors
image(LA_idw, "var1.pred", col = terrain.colors(20))

# Plot the contour lines on top of the image
contour(LA_idw, "var1.pred", add = TRUE)

##########

# Task 4.3: Trend Surfaces

# Perform a kriging interpolation of the topo data: Develop a surface for optimum weights of the interpolation
  # z~1 = simple and ordinary formula
  # newdata = grid with prediction lcations
  # degree = 1 (linear)
LA_tsa_1 <- krige(Z~1, LA_datatable, newdata = LA_grid, degree = 1)

# Convert grided kriging into image format
im <- as.image.SpatialGridDataFrame(LA_tsa_1)

# Plot the image of the data with 5 colors
image(LA_tsa_1,"var1.pred",col=terrain.colors(5))

# Convert from contour lines to spatial lines data frame object
LA_tsa_line <-ContourLines2SLDF(contourLines(im))

# Plot the contour lines on top of the image
contour(LA_tsa_1,"var1.pred", add=TRUE)

# Perform a kriging interpolation of the topo data: Develop a surface for optimum weights of the interpolation
  # z~1 = simple and ordinary formula
  # newdata = grid with prediction lcations
  # degree = 2 (quadratic!!)
LA_tsa_2 <- krige(Z~1, LA_datatable, newdata = LA_grid, degree = 2)

# Convert grided kriging into image format
im <- as.image.SpatialGridDataFrame(LA_tsa_2)

# Plot the image of the data with 5 colors
image(LA_tsa_2,"var1.pred",col=terrain.colors(5))

# Convert from contour lines to spatial lines data frame object
LA_tsa_line <-ContourLines2SLDF(contourLines(im))

# Plot the contour lines on top of the image
contour(LA_tsa_2,"var1.pred", add=TRUE)

# Read the data table, there is a header
LA_dat <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\Simple_LA_OZ.txt", header = TRUE)
x <- LA_dat$X
y <- LA_dat$Y
z <- LA_dat$Z
linear_surf <- lm(z ~ x + y)
linear_surf
quad_surf <- lm(z ~ x + y + I(x * x) + I(y * y) + I(x * y))
quad_surf

hist(linear_surf$residuals)

summary(linear_surf)

##########

# Task 4.8: Variography and Interpolation by Kriging
# http://rstudio-pubs-static.s3.amazonaws.com/9688_a49c681fab974bbca889e3eae9fbb837.html

library(sp)
library(gstat)
library(maptools)

# Read the data table, there is a header
LA_datatable_rm31 <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\Simple_LA_OZ_rm31.txt", header = TRUE)

# Set spatial coordinates of the data 
coordinates(LA_datatable_rm31) <- c("X", "Y")

# Sample point locations of the data, with sample size 3720, using a regular (systematically aligned) sampling method
LA_grid <- spsample(LA_datatable_rm31, "regular", n = 3720)

# Add a Gridded topology to the sampled points
gridded(LA_grid) <- TRUE

# Make the grid full and ordered (full matrix form)
fullgrid(LA_grid) <- TRUE

# Perform an Inverse distance weighted (IDW) interpolation on the LA data
  # weight nearer points over more distant points
  # Z~1 = simple and ordinary formula
  # newdata = grid with prediction locations
  # Inverse distance weighting power = 2
LA_idw <- idw(Z~1, LA_datatable_rm31, newdata = LA_grid, idp = 2.0)

# Convert the gridded idw into image format
im <- as.image.SpatialGridDataFrame(LA_idw)

# Convert from contour lines to spatial lines data frame object
LA_SLDF <- ContourLines2SLDF(contourLines(im))

# Create a list of 2 objects
LA_spl <- list("sp.lines", LA_SLDF)

# Plot the interpolation with the variable pred, and the contour lines on top
spplot(LA_idw, "var1.pred", sp.layout = LA_spl)

# Plot the image of the data with 20 colors
image(LA_idw, "var1.pred", col = terrain.colors(20))

# Plot the contour lines on top of the image
contour(LA_idw, "var1.pred", add = TRUE)

#####

# Plot the variogram cloud
LA_vg <- variogram(Z~1, data = LA_datatable_rm31, cloud = TRUE)
plot(LA_vg)

# See where outlier pairs of values occur on the map

# Select area in plot for analysis, and identify point pairs in the area
LASV_edit <- plot(variogram(Z~1, LA_datatable_rm31, cloud = TRUE), digitize = TRUE)
plot(LASV_edit, LA_datatable_rm31)

LA_vg <- variogram(Z~1, data = LA_datatable_rm31)
plot(LA_vg)

# Get quantities contained in the variogram
LA_vg

# Examine the variation stratified by direction
plot(variogram(Z~1, LA_datatable_rm31, alpha = c(0,45,90,135)))

#####

# Task 4.6: Spatial Inperpolation by Kriging

# List some variogram functions available in gstat
vgm()

# Spherical
vgm(1, "Sph", range = 390)

# Linear
vgm(0, "Lin")

# Occam's Razor
top_v_fit <- fit.variogram(LA_vg, vgm(2000, model = "Sph", range = 2.5, nugget = 0.0))

# Perform automatic Kriging on given dataset and generate experimental variogram
d <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\Simple_LA_OZ_rm31.txt", header = T)
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

#####

# Interpolation By Kriging

LA_map_OK <- krige(Z~1, LA_datatable_rm31, LA_grid, top_v_fit)
LA_im <- as.image.SpatialGridDataFrame(LA_map_OK)
LA_SLDF <- ContourLines2SLDF(contourLines(LA_im))
LA_sp1 <- list("sp.lines", LA_SLDF)
image(LA_map_OK, "var1.pred", col = terrain.colors(20))
contour(LA_map_OK, "var1.pred", add = TRUE)

# Variance estimates can also be mapped
# Used to suggest where additional data are/are not required
image(LA_map_OK, "var1.var", col = terrain.colors(20))
contour(LA_map_OK, "var1.var", add = TRUE)
