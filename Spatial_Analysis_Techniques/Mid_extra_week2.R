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

# Open jpeg image in plotting window
cal <- ReadAndCal("N:\\USERS\\Avery\\Spatial_Analysis_Techniques_in_R\\Data\\something.jpg")

