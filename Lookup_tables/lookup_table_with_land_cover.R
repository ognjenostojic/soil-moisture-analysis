# Set working directory
setwd("/home/xosto002/Beachelor_thesis")

# Load the original lookup table
lookup_table <- read.csv("Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv",
                         stringsAsFactors = FALSE)

# Add a new empty column for Land Cover
lookup_table$Land_Cover <- NA

# Loop through each row (station) of lookup table
for (i in 1:nrow(lookup_table)) {
  
  # Extract directory path from Filename
  filepath <- lookup_table$Filename[i]
  
  # Get directory path of the station
  station_dir <- dirname(filepath)
  
  # List CSV files in the station directory
  csv_files <- list.files(path = station_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Proceed only if there's at least one CSV file
  if (length(csv_files) > 0) {
    # Read the first CSV file (assuming there's at least one CSV per station)
    station_csv <- read.csv(csv_files[1], sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
    
    # Find the rows containing "land cover classification"
    land_cover_rows <- station_csv[station_csv$quantity_name == "land cover classification", ]
    
    # Check if land cover info is present
    if (nrow(land_cover_rows) > 0) {
      # Take the most recent land cover classification (assuming the latest year is at the bottom)
      land_cover <- tail(land_cover_rows$description, 1)
      
      # Add the extracted land cover to the lookup table
      lookup_table$Land_Cover[i] <- land_cover
    }
  } else {
    # If no CSV file is found, keep Land_Cover as NA
    lookup_table$Land_Cover[i] <- NA
  }
  
  # Progress message
  cat("Processed station ID:", lookup_table$ID[i], "Land cover:", lookup_table$Land_Cover[i], "\n")
}

# Save the updated lookup table
write.csv(lookup_table, 
          file = "Project/Lookup_tables/lookup_table_with_land_cover.csv",
          row.names = FALSE)

cat("✅ Updated lookup table saved with Land Cover information.\n")
