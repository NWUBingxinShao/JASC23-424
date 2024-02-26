# Based on the reconstruction curves for the periods 1961-2021 and 4000-4000 cal yr BP having roughly similar means, we standardize them using Z-scores and then apply perturbations.
# Combining the upper bound standard score (Z_Upper+2.134) and lower bound standard score (Z_Lower-0.476), we determine the extent of perturbation.
# Load the required libraries
library(raster)
library(tidyverse)

# Define a function to process data files, including reading, writing, and temperature adjustments
process_data <- function(input_path, output_path, adjustment) {
  if (!file.exists(output_path)) {
    dir.create(output_path)
  }
  
  files <- list.files(input_path, pattern = "\\.csv$", full.names = TRUE)
  
  for (f in files) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$TMAX <- df$TMAX + adjustment
    df$TMIN <- df$TMIN + adjustment
    df$TAVG <- df$TAVG + adjustment
    file_name <- basename(f)
    new_name <- paste0(file_name, "gdd", adjustment, ".csv")
    new_file_path <- file.path(output_path, new_name)
    write.csv(df, new_file_path, row.names = FALSE)
  }
}

# Data preparation process
# Specify the path to the .tif file
tif_file <- "YOUR_TIF_FILE_PATH_HERE"  # Replace with your .tif file path

# Read the .tif file
raster_obj <- raster(tif_file)

# Get the extent coordinates
extent_coords <- extent(raster_obj)

# Print the coordinates of the east, west, south, and north boundaries
cat("East Boundary Coordinates:", extent_coords@xmax, "\n")
cat("West Boundary Coordinates:", extent_coords@xmin, "\n")
cat("South Boundary Coordinates:", extent_coords@ymin, "\n")
cat("North Boundary Coordinates:", extent_coords@ymax, "\n")

# Use boundary data to fetch relevant station information from the NOAA website.
# If the FedDATA package is up to date, you can directly use it to fetch meteorological station information.

# Set file paths
input_paths <- c(
  "YOUR_INPUT_PATH_1_HERE",  # Replace with the path to the first input folder
  "YOUR_INPUT_PATH_2_HERE"   # Replace with the path to the second input folder
)

output_paths <- c(
  "YOUR_OUTPUT_PATH_1_HERE",  # Replace with the path to the first output folder
  "YOUR_OUTPUT_PATH_2_HERE"   # Replace with the path to the second output folder
)

adjustments <- c(-0.476, 2.134)  # Adjust temperatures as needed

# Process data files
for (i in 1:length(input_paths)) {
  process_data(input_paths[i], output_paths[i], adjustments[i])
}

# Specify the folder path
folder_path <- "YOUR_FOLDER_PATH_HERE"  # Replace with your folder path

# Create an output folder
output_folder <- file.path(folder_path, "Processed")
dir.create(output_folder, showWarnings = FALSE)

# Get a list of files
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Define a function to handle NA values
replace_na_with_closest_valid <- function(x, date) {
  na_indices <- which(is.na(x))
  
  for (i in na_indices) {
    valid_indices <- which(!is.na(x))
    closest_valid_idx <- valid_indices[which.min(abs(date[valid_indices] - date[i]))]
    x[i] <- x[closest_valid_idx]
  }
  
  return(x)
}

# Define a function to process TAVG and Tn
process_tavg_and_tn <- function(data) {
  Tn <- (data$TMAX + data$TMIN) / 2  # Calculate daily average temperature Tn
  data$TAVG <- pmax(data$TAVG, Tn)    # Compare TAVG and Tn values and keep the larger one
  data$TAVG <- replace_na_with_closest_valid(data$TAVG, data$DATE)  # Replace NA values in TAVG
  return(data)
}

# Iterate through the file list, read, handle NA values, TAVG, and Tn, and output processed data
for (file in file_list) {
  data <- read_csv(file)
  
  data$TMAX <- replace_na_with_closest_valid(data$TMAX, data$DATE)
  data$TMIN <- replace_na_with_closest_valid(data$TMIN, data$DATE)
  
  data <- process_tavg_and_tn(data)
  
  # Output processed data to a file
  output_path <- file.path(output_folder, basename(file))
  write_csv(data, file = output_path)
  
  cat("Processed and updated:", file, "\n")
}

cat("All files processed and updated.\n")

# Load the required libraries
library(tidyverse)

# Specify the folder path
folder_path <- "YOUR_FOLDER_PATH_HERE"  # Replace with your folder path

# Get a list of files
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Define the reference temperature Tb
Tb <- "your result"

# Define a function to add the GDD column
add_gdd_column <- function(data) {
  data$GDD <- data$TAVG - Tb
  return(data)
}

# Iterate through the file list, read the file, and add the GDD column
for (file in file_list) {
  data <- read_csv(file)
  data <- add_gdd_column(data)
  
  # Rewrite the data with the new GDD column to the same file
  write_csv(data, file)
  
  cat("GDD column added to:", file, "\n")
}

cat("GDD columns added to all files.\n")

# Load the required libraries
library(tidyverse)

# Specify the original folder path and the new folder path
input_folder <- "YOUR_INPUT_FOLDER_PATH_HERE"  # Replace with the original folder path
output_folder <- "YOUR_OUTPUT_FOLDER_PATH_HERE"  # Replace with the new folder path
dir.create(output_folder, showWarnings = FALSE)

# Get the original file list
file_list <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Define the reference temperature Tb and start and end months
Tb <- "your result"
start_month <- 4
end_month <- 9

# Define a function to convert pure numerical DATE to a date
convert_to_date <- function(date_num) {
  start_date <- as.Date("1951-01-01")  # Start date corresponding to pure numerical value
  converted_date <- start_date + as.difftime(date_num - 18628, units = "days")
  return(converted_date)
}

# Define a function to calculate GDDS within the specified date range
calculate_gdds <- function(data) {
  data %>%
    mutate(DATE = convert_to_date(DATE),
           YEAR = year(DATE)) %>%
    filter(month(DATE) >= start_month, month(DATE) <= end_month) %>%
    mutate(GDD = pmax(TAVG - Tb, 0)) %>%
    group_by(NAME, YEAR) %>%
    summarize(GDDS = sum(GDD))
}

# Iterate through the file list, read the file, calculate GDDS, and write the data to the new folder
for (file in file_list) {
  data <- read_csv(file)
  gdds_data <- calculate_gdds(data)
  
  # Extract station information
  site_info <- data %>%
    select(LATITUDE, LONGITUDE, ELEVATION, NAME) %>%
    distinct()
  
  # Merge station information, GDDS, and years
  result_data <- merge(site_info, gdds_data, by = "NAME", all.x = TRUE)
  
  # Write the merged data to the new folder
  output_file <- file.path(output_folder, paste0(basename(file), "_GDDS.csv"))
  write_csv(result_data, file = output_file)
  
  cat("GDDS data calculated and saved:", output_file, "\n")
  
  # Remove data for the year 2023
  result_data <- result_data %>%
    filter(YEAR != 2023)
  
  # Update the file
  write_csv(result_data, file = output_file, append = FALSE)
}

cat("GDDS calculations and saving completed.\n")

# Specify the original folder path
input_folder <- "YOUR_INPUT_FOLDER_PATH_HERE"  # Replace with the original folder path

# Get the list of GDDS files
file_list <- list.files(path = input_folder, pattern = "_GDDS.csv$", full.names = TRUE)

# Define a function to calculate the annual average GDDS for stations (MGDDS) and exclude outliers
calculate_mgdds <- function(data) {
  data %>%
    group_by(YEAR) %>%
    mutate(MGDDS = mean(GDDS, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(MGDDS))  # Exclude years containing NA values
}

# Store data for all stations
all_station_data <- data.frame()

# Iterate through the GDDS file list, read the files, and calculate MGDDS
for (file in file_list) {
  data <- read_csv(file)
  mgdds_data <- calculate_mgdds(data)
  
  # Add station data to the overall data
  all_station_data <- bind_rows(all_station_data, mgdds_data)
  
  cat("MGDDS calculated for:", file, "\n")
}

# Summarize information for all stations and calculate the annual average MGDDS
summary_data <- all_station_data %>%
  group_by(NAME, LATITUDE, LONGITUDE, ELEVATION) %>%
  summarize(MGDDS = mean(MGDDS, na.rm = TRUE))

# Write the summary data to a new file
summary_file <- file.path(input_folder, "all-station.csv")
write_csv(summary_data, file = summary_file)

cat("Summary data saved to:", summary_file, "\n")

# Due to the complexity of R code for generating kriging interpolation plots,
# this process uses ArcGIS for interpolation with mostly default parameters.
