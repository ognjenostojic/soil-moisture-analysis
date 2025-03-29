# Load necessary libraries
library(dplyr)
library(stringr)

# Set the top-level directory containing all files
top_dir <- "/home/xosto002/Beachelor_thesis"

# Get a list of all STM files in subdirectories
stm_files <- list.files(top_dir, pattern = "\\.stm$", full.names = TRUE, recursive = TRUE)

# Initialize an empty data frame to store the lookup table
lookup_table <- data.frame(
  ID = integer(),
  Name = character(),
  Station = character(),
  Latitude = numeric(),
  Longitude = numeric(),
  Elevation = numeric(),
  Depth = character(),
  Avg_Depth = numeric(),
  Beginning_Year = integer(),
  Ending_Year = integer(),
  Missing_Years = character(),
  Percentage_Missing_Values = numeric(),
  Number_of_Full_Years = integer(),
  Filename = character(),  # Add a column to store the filename
  stringsAsFactors = FALSE
)

# Function to read an STM file and extract lookup data
process_stm_file <- function(file, id) {
  # Read the STM file
  stm_lines <- readLines(file)
  
  # Extract basic metadata from the first line
  header <- unlist(strsplit(stm_lines[1], "\\s+"))
  
  # Extract substation and station name from the file path
  name <- basename(dirname(file))  # Assuming substation folder name as Name
  station <- basename(dirname(dirname(file)))  # Assuming station folder name as Station
  
  # Extract latitude, longitude, elevation, and depth from the first line
  latitude <- as.numeric(header[4])
  longitude <- as.numeric(header[5])
  elevation <- as.numeric(header[6])
  depth1 <- as.numeric(header[7])
  depth2 <- as.numeric(header[8])
  avg_depth <- mean(c(depth1, depth2), na.rm = TRUE)
  depth <- paste(depth1, depth2, sep = "-")
  
  # Extract beginning and ending years from the data rows
  dates <- as.Date(sapply(stm_lines[-1], function(line) strsplit(line, "\\s+")[[1]][1]), format = "%Y/%m/%d")
  beginning_year <- as.numeric(format(min(dates, na.rm = TRUE), "%Y"))
  ending_year <- as.numeric(format(max(dates, na.rm = TRUE), "%Y"))
  
  # Check for missing years
  all_years <- seq(beginning_year, ending_year)
  observed_years <- unique(as.numeric(format(dates, "%Y")))
  missing_years <- setdiff(all_years, observed_years)
  missing_years_str <- if (length(missing_years) > 0) paste(missing_years, collapse = ", ") else NA
  
  # Calculate the percentage of missing years
  percentage_missing_values <- length(missing_years) / length(all_years) * 100
  
  # Calculate the number of full years
  number_of_full_years <- length(observed_years)
  
  # Return a data frame row for this file, including the full filename
  data.frame(
    ID = id,
    Name = name,
    Station = station,
    Latitude = latitude,
    Longitude = longitude,
    Elevation = elevation,
    Depth = depth,
    Avg_Depth = avg_depth,
    Beginning_Year = beginning_year,
    Ending_Year = ending_year,
    Missing_Years = missing_years_str,
    Percentage_Missing_Values = percentage_missing_values,
    Number_of_Full_Years = number_of_full_years,
    Filename = file,  # Store the full path to the file
    stringsAsFactors = FALSE
  )
}

# Loop over each file and build the lookup table
for (i in seq_along(stm_files)) {
  file <- stm_files[i]
  lookup_table <- bind_rows(lookup_table, process_stm_file(file, i))
}

# Save the lookup table to a CSV file
write.csv(lookup_table, "/home/xosto002/Beachelor_thesis/lookup_table_with_filenames.csv", row.names = FALSE)

# Print the lookup table
print(lookup_table)
