##########
# Assignment Week 2
##########

# Author: Avery Sandborn
# Date: February 9, 2016

##########

# Load spatstat library
library(spatstat)

# Summarize New Zealand Trees data
data(nztrees)
summary(nztrees)
class(nztrees) # already ppp object

# Define bounding Box
xlim <- c(0.0, 1.0)
ylim <- c(0.0, 1.0)
attach(nztrees)

# Plot points of nztrees with bounding box
plot(nztrees)

# Nearest Neighbor Test 
  # Examine clark and evans nearest neighbor index of aggregation R
  # Crude measure of cluserting or ordering of a point pattern 
clarkevans(nztrees)
# R is ~1.05 >> close to random, more uniform than clustered

# visualization using KDE
dmap <- density(nztrees, 10, at = "pixels")
plot(dmap)

# Find optumal bandwidth for a given pattern
bw <- bw.diggle(nztrees)
plot(bw)
bw2 <- as.numeric(bw)
bw2
# bw2 is ~5.6 

# KDE visualization with bet bandwidth and points added
dmap <- density(nztrees, 5.6, at = "pixels")
plot(dmap)
plot(nztrees, add = TRUE)

# Show range of variation in density seen at this bandwidth
contour(dmap, add = TRUE)
persp(dmap)

########################

library(spatstat)

# Summarize anemone data
data(anemones)
summary(anemones)
class(anemones) # already ppp object
attach(anemones)

# Plot points of anemones with bounding box
plot(anemones)

# Nearest Neighbor Test 
# Examine clark and evans nearest neighbor index of aggregation R
# Crude measure of cluserting or ordering of a point pattern 
clarkevans(anemones)
# R is ~1.39 >> uniform

# visualization using KDE
dmap <- density(anemones, 10, at = "pixels")
plot(dmap)

# Find optumal bandwidth for a given pattern
bw <- bw.diggle(anemones)
plot(bw)
bw2 <- as.numeric(bw)
bw2
# bw2 is ~11.18 

# KDE visualization with bet bandwidth and points added
dmap <- density(anemones, 11.18, at = "pixels")
plot(dmap)
plot(anemones, add = TRUE)

# Show range of variation in density seen at this bandwidth
contour(dmap, add = TRUE)
persp(dmap)

# Create a historgram of the diameter of the anemones
marks <- anemones$marks
marks
hist(marks, breaks = 1:8, col = "gray", labels = TRUE)
