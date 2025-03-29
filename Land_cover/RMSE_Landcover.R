# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)

# Set working directory and define paths
setwd("/home/xosto002/Beachelor_thesis")
output_dir <- "/home/xosto002/Beachelor_thesis/Project/BoxPlots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Read lookup table with land cover information
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/Project/Lookup_tables/lookup_table_with_land_cover.csv")

# Define simulation file paths
simulation_dir_main <- "/DATA/theses/ognjen/mhm_timeseries_1998_2022"
simulation_dir_1km <- "/DATA/theses/ognjen/mhm_timeseries_1998_2022_NEW"

# Define resolutions at Level 1
resolutions <- c("0p015625deg" = "1km", "0p03125deg" = "3km", "0p0625deg" = "5km", "0p125deg" = "11km", "0p25deg" = "22km")

# Filter stations that contain 'Cropland' or 'Forest' in their land cover
filtered_stations <- lookup_table %>%
  filter(grepl("Cropland", Land_Cover, ignore.case = TRUE) | grepl("Tree cover|Forest", Land_Cover, ignore.case = TRUE))

# Function to read and normalize STM (observation) data
read_and_clean_stm <- function(filename) {
  if (!file.exists(filename)) return(NULL)
  data <- read.table(filename, header = FALSE, sep = " ", skip = 1, col.names = c("Date", "Time", "Value", "Quality", "Mode"))
  data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y/%m/%d %H:%M", tz = "UTC")
  data$Value <- as.numeric(data$Value)
  data <- data %>% mutate(Value = (Value - min(Value, na.rm = TRUE)) / (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE)))
  return(data[, c("Datetime", "Value")])
}

# Function to read and normalize NetCDF simulation data
read_nc_simulation <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  nc_data <- nc_open(file_path)
  sim_values <- ncvar_get(nc_data, names(nc_data$var)[1])
  sim_values <- (sim_values * ncatt_get(nc_data, names(nc_data$var)[1], "scale_factor")$value) +
    ncatt_get(nc_data, names(nc_data$var)[1], "add_offset")$value
  sim_values[sim_values == ncatt_get(nc_data, names(nc_data$var)[1], "_FillValue")$value] <- NA
  dates <- as.POSIXct(ncvar_get(nc_data, "time") * 3600, origin = "1998-01-01", tz = "UTC")
  sim_values <- (sim_values - min(sim_values, na.rm = TRUE)) / (max(sim_values, na.rm = TRUE) - min(sim_values, na.rm = TRUE))
  nc_close(nc_data)
  return(data.frame(Datetime = dates, Simulated_Value = sim_values))
}

# Function to calculate RMSE
calculate_rmse <- function(obs, sim) {
  common_dates <- intersect(obs$Datetime, sim$Datetime)
  obs_filtered <- obs[obs$Datetime %in% common_dates, ]
  sim_filtered <- sim[sim$Datetime %in% common_dates, ]
  if (nrow(obs_filtered) == 0 || nrow(sim_filtered) == 0) return(NA)
  sqrt(mean((obs_filtered$Value - sim_filtered$Simulated_Value)^2, na.rm = TRUE))
}

# Ensure results dataframe has the Land_Cover column
results <- data.frame(Station_ID = integer(), Resolution = character(), RMSE = numeric(), Land_Cover = character(), stringsAsFactors = FALSE)

# Loop through filtered stations
for (station_id in unique(filtered_stations$ID)) {
  stm_file_path <- unique(filtered_stations$Filename[filtered_stations$ID == station_id])[1]
  obs_data <- read_and_clean_stm(stm_file_path)
  if (is.null(obs_data)) next
  
  # Fetch Land_Cover and ensure it is valid
  land_cover <- unique(filtered_stations$Land_Cover[filtered_stations$ID == station_id])
  if (length(land_cover) == 0) land_cover <- "Unknown"
  land_cover <- paste(land_cover, collapse = ", ")  # Ensure it's a single string
  
  for (res in names(resolutions)) {
    sim_file_path <- ifelse(res == "0p015625deg",
                            sprintf("%s/SMvol_L01_%s_ID_%d_new.nc", simulation_dir_1km, res, station_id),
                            sprintf("%s/SMvol_L01_%s_ID_%d.nc", simulation_dir_main, res, station_id))
    
    sim_data <- read_nc_simulation(sim_file_path)
    if (!is.null(sim_data)) {
      rmse_value <- calculate_rmse(obs_data, sim_data)
      results <- rbind(results, data.frame(Station_ID = station_id, Resolution = resolutions[res], RMSE = rmse_value, Land_Cover = land_cover))
    }
  }
}

# Save results to a CSV file
write.csv(results, sprintf("%s/RMSE_Land_Cover_Level1.csv", output_dir), row.names = FALSE)

# Convert resolution to factor for proper ordering
results$Resolution <- factor(results$Resolution, levels = c("1km", "3km", "5km", "11km", "22km"))

# Ensure Land_Cover exists before filtering
if ("Land_Cover" %in% colnames(results) && nrow(results) > 0) {
  cropland_results <- results %>% filter(grepl("Cropland", Land_Cover, ignore.case = TRUE))
  forest_results <- results %>% filter(grepl("Tree cover|Forest", Land_Cover, ignore.case = TRUE))
} else {
  cat("⚠️ Warning: 'Land_Cover' column missing from results. No plots will be generated.\n")
  cropland_results <- NULL
  forest_results <- NULL
}

# Define colors for the plots
color_map <- c("1km" = "#E41A1C", "3km" = "#A9A900", "5km" = "#4DAF4A", "11km" = "#377EB8", "22km" = "#984EA3")

# Generate plots only if data is available
if (!is.null(cropland_results) && nrow(cropland_results) > 0) {
  ggplot(cropland_results, aes(x = Resolution, y = RMSE, fill = Resolution)) +
    geom_boxplot(alpha = 1) +
    scale_fill_manual(values = color_map) +
    labs(title = "RMSE for Cropland Across Resolutions (Level 1)", x = "Resolution", y = "RMSE") +
    theme_minimal()
  
  ggsave(sprintf("%s/RMSE_Cropland_Level1.pdf", output_dir), width = 10, height = 6)
}

if (!is.null(forest_results) && nrow(forest_results) > 0) {
  ggplot(forest_results, aes(x = Resolution, y = RMSE, fill = Resolution)) +
    geom_boxplot(alpha = 1) +
    scale_fill_manual(values = color_map) +
    labs(title = "RMSE for Forest Across Resolutions (Level 1)", x = "Resolution", y = "RMSE") +
    theme_minimal()
  
  ggsave(sprintf("%s/RMSE_Forest_Level1.pdf", output_dir), width = 10, height = 6)
}

cat("✅ Boxplots saved in:", output_dir, "\n")




land_cover <- unique(filtered_stations$Land_Cover[filtered_stations$ID == station_id])
print(results)
