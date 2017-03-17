# Append CSVs that have the same attribute setup
# Use when you want to keep attributes the same, but want to add the rows to the end of the files
# Iterates through a folder directory

# get file name variable
input_data_path = "N:/USERS/Avery/Soil_Moisture_Mapping/10032016/"
output_data_path = "N:/USERS/Avery/Soil_Moisture_Mapping/"
output_data_file = "10032016_allstates"
data_extension = ".csv"

# name the output file
out_file = paste(output_data_path, output_data_file, data_extension, sep="")

# add a header to the table
table_header = paste("Date", "State", "County", "Variable", "Value", ",", sep=",")

# create the output table
write.table(table_header, file = out_file, append = TRUE)

# for every file in the folder
for(x in list.files(path = input_data_path)) {

  # read the csv
  in_data = paste(input_data_path, x, sep="")
  print(x)
  in_csv = read.csv(in_data, header = F, sep = ",")
  
  #append the data to the output
  out_data <- cbind(in_csv)
  
  # overwrite the table and repeat!
  write.table(out_data, file = out_file, append = TRUE, sep = ",")
}

