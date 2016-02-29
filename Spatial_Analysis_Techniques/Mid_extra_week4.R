##########
# Assignment Week 4 - Mid Week
##########

# Author: Avery Sandborn
# Date: February 29, 2016

##########

# 3D Plots of Surface Data using rgl

library(rgl)

data(volcano)
class(volcano)

# Exaggerate the relief
y <- 2 * volcano

# Space the axes by assigning coordinates based on their (r, c) numbers
x <- 10 * (1:nrow(y)) # 10 units spacing south to north
z <- 10 * (1:ncol(y)) # 10 units spacing east to west
ylim <- range(y)
ylim

ylen <- ylim[2] - ylim[1] + 1
ylen

# Height color look up table
colorlut <- terrain.colors(ylen)

# Assign color to each height value
col <- colorlut[y - ylim[1] + 1]

# Open the output window
rgl.open()

# Plot the surface
rgl.surface(x, z, y, color = col, back = "lines")

# Input any interpolated values coerced into a matrix into this sequence

# Get heights from the object created by IDW
vals <- topo_idw$var1.pred

valmat <- matrix(data = vales, nrow = 60, ncol = 62)
