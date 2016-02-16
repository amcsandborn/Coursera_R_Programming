##########
# Assignment Week 2 - Mid Week
##########

# Author: Avery Sandborn
# Date: February 12, 2016

##########

library(spatstat)

# Create your own point pattern in spatstat

# Create pattern by clicking in window
my_dat <- clickppp(n=15)

# Examine clark and evans nearest neighbor index
clarkevans(my_dat)
summary(my_dat)

##########

# Digitizing data from old plots

# Install devtools and digitize
utils:::menuInstallPkgs()
require("devtools")
install_github("digitize", username="tpoisot", subdir="digitize")
library(digitize)

# Open jpeg image in plotting window, create tick points
  # In plotting window, click on right X, left X, bottom y, top y in that order
cal <- ReadAndCal("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\Harrington_with_turbines.jpg")
cal

# Capture Event Locations
  # Left click on each point to capture it
data.points <- DigitData(col = 'red')
data.points

# Calibration, ensure that the tick points are defining a unit square
df <- Calibrate(data.points, cal, 0, 1, 0, 1)
df

# Import data as a ppp object and plot
library(spatstat)
xlim <- c(0,1)
ylim <- c(0,1)
turbine <- ppp(df$x, df$y, xlim, ylim)
plot(turbine)
