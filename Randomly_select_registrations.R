
# Updated Search (Annie)

# Randomly order and select 2 more gambling study preregistrations

# load and read the data set containing the list of preregistrations
data2<- read.csv("Data_files/spreadsheet_name_saved_as_a.csv_file_in_Data_files_folder.csv")
head(data2) # Check the data has loaded correctly
names(data2) # Check the data has loaded correctly


# Randomly order gambling study preregistrations and select the first 2: 
data2[sample(nrow(data2), 2), ]


# ------------------------