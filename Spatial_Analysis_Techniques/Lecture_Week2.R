##########
# Lecture Week 2
##########

# Author: Avery Sandborn
# Date: February 8, 2016

##########

# Task 2.4: Are Drumlins Randomly Distributed?

# Getting the Data

library(spatstat)

# Read in the data
drumlin <- read.table("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\drumlins_in_unit_square.txt", header = TRUE)
drumlin
nrow(drumlin)
ncol(drumlin)
summary(drumlin)

# Define the bounding box for the data
xlim <- c(0.0, 1.0)
ylim <- c(0.0, 1.0)
attach(drumlin)

# Create a ppp object that spatstat can understand
drumlin_ppp <- ppp(X, Y, xlim, ylim)

# Plot the dot map of the drumlins
plot(drumlin_ppp)

# Changing the Bounding Box
  # almost always use rr version

# Bounding Box Alternative: Convex Hull of events
ch <- convexhull.xy(drumlin_ppp)

# Bounding Box Alternative: Ripley and Rasson's Maximum Likelihood estimate of Spatial domain from which the point events came
rr <- ripras(drumlin_ppp)

# Create ppp objects for each of these alternative bounding boxes
drumlin_ch <- ppp(drumlin_ppp$x, drumlin_ppp$y, window = ch)
drumlin_rr <- ppp(drumlin_ppp$x, drumlin_ppp$y, window = rr)

# plot the data with alternative bounding boxes
plot(drumlin_ch)
plot(drumlin_rr)

# The effect on a simple test for CSR (complete spatial randomness)

# Examine clark and evans nearest neighbor index of aggregation R
  # Crude measure of cluserting or ordering of a point pattern 
clarkevans(drumlin_ppp)
clarkevans(drumlin_ch)
clarkevans(drumlin_rr)
# R is arround 1.24 >> uniform

# Visualization using KDE
  # Generate kernel density plots for three different bandwidths

# Bandwidth = 0.5 >> very smooth estimate
dmap <- density(drumlin_ppp, 0.5, at = "pixels")
plot(dmap)

# Bandwidth = 0.1 >> roughens it up a bit
dmap <- density(drumlin_ppp, 0.1, at = "pixels")
plot(dmap)

# Bandwidth = 0.05 >> too low?
dmap <- density(drumlin_ppp, 0.05, at = "pixels")
plot(dmap)

# Guidence on bandwidth selection
bw = bw.diggle(drumlin_ppp)
plot(bw)

# Confirm which bandwidth to use
bw2 <- as.numeric(bw)
bw2 

# Distance Functions and their expected values under CSR

# Calculate and plot the estimated G function and its simulation envelope for the ppp object drumlin rr with 99 simulations
plot(envelope(drumlin_rr, fun = Gest, nsim = 99))
plot(envelope(drumlin_rr, Fest, nsim = 99))

# Estimate Ripley's K(d)
  # Most useful initial exploratory approach to point pattern analysis

# Get number of events in pattern from ppp object and check it's correct
n <- drumlin_rr$n
n

# Specify the way you want to simulate
  # Use expression to create an object called way
  # uses runifpoint to generate a random point pattern containing n independent random points with RR window 
way <- expression(runifpoint(n, win= rr))

runifpoint(n, win = owin(c(0,1), c(0,1)), giveup = 1000)
  # n = number of points to be generated
  # win = window in which to simulate pattern
  # owin = win is an object of class owin
  # giveup = number of attempts in rejection method after which the algorithm should stop trying to generate new points

# creat object kenv
kenv <- envelope(drumlin_rr, Kest, nsim = 99, simulate = way, verbose = FALSE)
plot(kenv)
  # contains a data frame with all values needed to enable a plot to be drawn
  # verbose = FALSE switches off reporting as the simulations are performed

plot(envelope(drumlin_rr, Lest, nsim=99))

drum_mad <- mad.test(drumlin_ppp, Kest)
drum_mad

drum_dclf <- dclf.test(drumlin_ppp, Kest)
drum_dclf

##################

# Task 2.5: Where and Why

# Summarize tropical tree data set
data(bei)
summary(bei)

# Does the intensity vary over the mapped region?
# Explore variations in estimated intensity by dividing the region into a regular pattern of quadrats and count how many events fall into each

# Create object q_map as density/intensity counts for a 3x3 grid of rectangular quadrats spanning the region
q_map <- quadratcount(bei, nx = 3, ny = 3)
q_map

# Plot points using a cross as a symbol
plot(bei, pch = "+")

# Create KDE estimates using bandwidth sigma of 65 distance units
kde_65 <- density(bei, sigma = 65.0)
plot(kde_65)
plot(bei, add = TRUE, pch = ".")

# Visualize the KDE surface as a contour type map and perpective diagram

# Tropical Trees KDE estimates
contour(kde_65, add = TRUE)

# Wire-frame display of densities
persp(kde_65)

# Create spatially continuous pixel image of the image class, and plot with a dot map of the events
kde_adapt <- adaptive.density(bei, f = 0.01, nrep = 10)
plot(kde_adapt)
plot(bei, add = TRUE, pch = ".")

#################

# Correcting for inhomogeneity using a covariate

# Slope gradient 
slope <- bei.extra$grad

# Plot histogram of the slope gradient
hist(slope)

slope_class <- quantile(slope, probs = (0:4)/4)
slope_class

# Convert these into levels of a factor and put them into slope_cut
slope_cut <- cut(slope, breaks = slope_class, labels = 1:4)
slope_cut

# Use tess to convert into a tessellation with four types of tile that we plote and add the original point event dot map to create a simple picture showing the events mapped into the four classes of the slope gradient
picture <- tess(image = slope_cut)
plot(picture)
plot(bei, add = TRUE, pch = ".")

# Confirm that trees are on steeper slope
count_by_slope <- quadratcount(bei, tess = picture)
count_by_slope

quadrat.test(bei, tess = picture)
