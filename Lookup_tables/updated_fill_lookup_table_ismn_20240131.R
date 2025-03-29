library(dplyr)

# Load the data from CSV files
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/lookup_table_with_filenames.csv")
fill_lookup_table <- read.csv("/DATA/theses/ognjen/processed/fill_lookup_table_ismn_20240131.csv")

# Merge the path information into fill_lookup_table based on IDs
# Using left_join to ensure all entries in fill_lookup_table are retained
merged_table <- fill_lookup_table %>%
  left_join(lookup_table %>% select(ID, Filename), by = "ID")

# Check the result
head(merged_table)

# Save the updated table back to a CSV file if needed
write.csv(merged_table, "/home/xosto002/Beachelor_thesis/updated_fill_lookup_table_ismn_20240131.csv", row.names = FALSE)
