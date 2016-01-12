##########
# County Correlations
##########

# Author: Avery Sandborn
# Date: 11/24/15

# Calculate R2 and RMSE values between national soybean yield estimates and each 
#   county in the Soybean States. Some counties do not have data for all years. Recode 
#   counties with 2 or less data points as NA for all calculations.  

# Helpful Websites
#   http://www.gardenersown.co.uk/education/lectures/r/correl.htm#correlation
#   http://www.statmethods.net/input/missingdata.html
#   http://stackoverflow.com/questions/6882709/how-do-i-deal-with-nas-in-residuals-in-a-regression-in-r
#   http://earlh.com/blog/2009/06/29/column-names-of-r-data-frames/

##########

# Install libraries
#install.packages("Metrics")
#library(Metrics)

# get file name variable
input_data_path = "D:/County_Correlations/soybean_yield/"
input_data_file = "soybean_county_1965-2014_ar_il_in_ia_ks_mn"
input_data_ext = ".csv"
output_data_file = "_Regression_v01"

input_data = paste(input_data_path, input_data_file, input_data_ext, sep = "")
output_data = paste(input_data_path, input_data_file, output_data_file, input_data_ext, sep = "")

# read csv into memory
county_data = read.csv(input_data)

# Attach data set so individual variables are read into memory
attach(county_data)

# Create header for table and write to CSV
table_header = paste(",", "State_County", "R2", "RMSE", "Count", ",", sep=",")
write.table(table_header, file = output_data, append = TRUE)

column_number = 1

# for each county...
for (x in county_data) {

  # Get count of how many data cells were used (not including NA values)
  count = length(which(x!="NA"))
  
  # If there are not enough data points, recode to NA
  if (count <= 2) {
    
    cor_pearson = "NA"
    R2_value = "NA"
    RMSE_value = "NA"
    
  }
  
  # If there are enough data points, proceed with calculations
  else {
  
    # Calculate R2 and RMSE
    cor_pearson = cor(National, x, use = "complete.obs")
    R2_value = cor_pearson * cor_pearson
    RMSE_value = sqrt(mean((x-National)^2, na.rm=TRUE))
  
  }
  
  # Set up the CSV file right
  header = toString(colnames(county_data)[column_number])
  out_data = paste(",", header, R2_value, RMSE_value, count, ",", sep = ",")

  # write/append number to csv table
  write.table(out_data, file = output_data, append = TRUE)
  
  # Repeat for next county
  column_number = column_number + 1
  
}

