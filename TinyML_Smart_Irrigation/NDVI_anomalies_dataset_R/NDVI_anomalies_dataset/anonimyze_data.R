#########################
#NDVI Summaries#
#########################
# Load necessary libraries
library(dplyr)
library(readr)
library(purrr)
# Set the path to the directory containing the CSV files
path <- file.path("datos","verdor","summaries","NDVI","scl_7_8_9")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_NDVI_scl_7_8_9.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")



#########################
#Irrigation Data consumptions#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","riego","consumos")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_irrigation_data.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")

#########################
#Irrigation Data Lectures#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","riego","lecturas")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_irrigation_data_lectures.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")

#########################
#NDVI_INT_1#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","int","agg_tall_NDVI_NA_Mean")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_int_agg_tall_NDVI_NA_Mean.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")

#########################
#NDVI_INT_2#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","int","agg_tall_NDVI_scl_7_8_9_Mean")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_int_agg_tall_NDVI_scl_7_8_9_Mean.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")
#########################
#NDVI_INT_3#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","int","redim_tall_NDVI_scl_7_8_9_Mean")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_int_redim_tall_NDVI_scl_7_8_9_Mean.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")
#########################
#NDVI_INT_4#
#########################

# Set the path to the directory containing the CSV files
path <- file.path("datos","int","redim_tall_NDVI_NA_Mean")

# List all CSV files in the directory
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a dataframe to store the correspondence between old and new filenames
correspondence <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)

# Loop over the files to rename them
for (i in seq_along(files)) {
  # Extract the original filename (without path and extension)
  original_filename <- tools::file_path_sans_ext(basename(files[i]))
  
  # Generate the new filename
  new_filename <- paste(i, ".csv", sep = "")
  
  # Rename the file
  file.rename(from = files[i], to = file.path(path, new_filename))
  
  # Add the correspondence to the dataframe
  correspondence <- rbind(correspondence, data.frame(old_name = original_filename, new_name = new_filename))
}

# Write the correspondence to a CSV file
write_csv(correspondence, file.path(path, "correspondence_int_redim_tall_NDVI_NA_Mean.csv"))

# Print completion message
print("Files have been renamed and correspondence file has been created.")
