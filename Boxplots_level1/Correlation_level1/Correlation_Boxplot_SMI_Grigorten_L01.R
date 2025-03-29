# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)
library(lfstat)  # For gringorten function

# Set working directory and define output path
setwd("/home/xosto002/Beachelor_thesis")
output_dir <- "/home/xosto002/Beachelor_thesis/Project/BoxPlots/Grigorten_Correlation_L01"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read lookup table containing all stations
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv", stringsAsFactors = FALSE)

# Define simulation file paths
simulation_dir_main <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022"
simulation_dir_1km <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER"

# Define Level 1 resolutions
resolutions <- c("0p015625deg" = "1km",  # Taken from NEW folder
                 "0p03125deg"  = "3km", 
                 "0p0625deg"   = "5km", 
                 "0p125deg"    = "11km", 
                 "0p25deg"     = "22km")

# Function to compute SMI using Gringorten plotting positions
calculate_smi_gringorten <- function(values) {
  if (length(values) < 5 || all(is.na(values))) return(rep(NA, length(values)))
  smi_values <- gringorten(values)
  return(smi_values)
}

# Function to read and compute SMI for STM (observation) data
read_and_clean_stm <- function(filename) {
  if (!file.exists(filename)) {
    cat("⚠️ Missing STM file:", filename, "\n")
    return(NULL)
  }
  
  lines <- readLines(filename)
  data <- read.table(text = lines[-1], sep = " ", fill = TRUE, header = FALSE)
  names(data) <- c("Date", "Time", "Value", "Quality", "Mode")
  data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y/%m/%d %H:%M", tz = "UTC")
  data$Value <- as.numeric(data$Value)
  
  # Remove NAs before calculating SMI
  data <- data %>% filter(!is.na(Value))
  
  # Apply Gringorten method to get SMI
  data$SMI <- calculate_smi_gringorten(data$Value)
  
  return(data[, c("Datetime", "SMI")])
}

# Function to read and compute SMI for NetCDF simulation data
read_nc_simulation <- function(file_path, resolution, obs_start, obs_end) {
  if (!file.exists(file_path)) {
    cat("⚠️ Missing NetCDF file:", file_path, "\n")
    return(NULL)
  }
  
  nc_data <- nc_open(file_path)
  var_name <- names(nc_data$var)[1]
  sim_values <- ncvar_get(nc_data, var_name)
  times <- ncvar_get(nc_data, "time")
  
  scale_factor <- ncatt_get(nc_data, var_name, "scale_factor")$value
  add_offset <- ncatt_get(nc_data, var_name, "add_offset")$value
  fill_value <- ncatt_get(nc_data, var_name, "_FillValue")$value
  
  dates <- as.POSIXct(times * 3600, origin = "1998-01-01", tz = "UTC")
  
  # Apply scale correction
  sim_values <- (sim_values * scale_factor) + add_offset
  sim_values[sim_values == fill_value] <- NA
  
  # Remove all NA cases
  valid_indices <- !is.na(sim_values)
  if (sum(valid_indices) == 0) {
    cat("⚠️ No valid data in:", file_path, "\n")
    return(NULL)
  }
  
  # Filter simulation data to match observation period
  sim_data <- data.frame(Datetime = dates, Simulated_Value = sim_values, Resolution = resolution)
  sim_data <- sim_data %>% filter(Datetime >= obs_start & Datetime <= obs_end)
  
  # Compute SMI using Gringorten method
  sim_data$SMI <- calculate_smi_gringorten(sim_data$Simulated_Value)
  
  nc_close(nc_data)
  
  return(sim_data)
}

# Function to calculate correlation
calculate_correlation <- function(obs, sim) {
  common_dates <- intersect(obs$Datetime, sim$Datetime)
  obs_filtered <- obs %>% filter(Datetime %in% common_dates)
  sim_filtered <- sim %>% filter(Datetime %in% common_dates)
  
  if (nrow(obs_filtered) == 0 || nrow(sim_filtered) == 0) {
    return(NA)
  }
  
  return(cor(obs_filtered$SMI, sim_filtered$SMI, use = "complete.obs"))
}

# Create a dataframe to store correlation results
correlation_results <- data.frame()

# Loop through real station IDs in the lookup table
for (station_id in unique(lookup_table$ID)) {
  # Ensure we get only one STM file per station
  stm_file_path <- unique(lookup_table$Filename[lookup_table$ID == station_id])[1]
  obs_data <- read_and_clean_stm(stm_file_path)
  
  if (is.null(obs_data)) {
    next
  }
  
  obs_start <- min(obs_data$Datetime, na.rm = TRUE)
  obs_end <- max(obs_data$Datetime, na.rm = TRUE)
  
  for (res in names(resolutions)) {
    sim_file_path <- ifelse(res == "0p015625deg",
                            sprintf("%s/SMvol_L01_%s_ID_%d_new.nc", simulation_dir_1km, res, station_id),  # NEW folder for 1km
                            sprintf("%s/SMvol_L01_%s_ID_%d.nc", simulation_dir_main, res, station_id))  # Main folder for others
    
    sim_data <- read_nc_simulation(sim_file_path, res, obs_start, obs_end)
    
    if (!is.null(sim_data)) {
      correlation_value <- calculate_correlation(obs_data, sim_data)
      correlation_results <- rbind(correlation_results, 
                                   data.frame(Station_ID = station_id, 
                                              Resolution = resolutions[res], 
                                              Correlation = correlation_value))
    }
  }
}

# Save correlation results to a CSV file
write.csv(correlation_results, sprintf("%s/Correlation_results_SMI_L01.csv", output_dir), row.names = FALSE)

# **Ensure Correct Ordering of Resolutions in Boxplot**
resolution_order <- c("1km", "3km", "5km", "11km", "22km")

# Generate Boxplot with Ordered Resolutions
ggplot(correlation_results, aes(x = factor(Resolution, levels = resolution_order), y = Correlation, fill = Resolution)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
  scale_fill_manual(values = c("1km" = "#E41A1C", "3km" = "#377EB8", "5km" = "#4DAF4A", "11km" = "#FF7F00", "22km" = "#984EA3")) +
  labs(title = "Correlation Boxplot for Different Resolutions (Level 1) - SMI", x = "Resolution", y = "Correlation (SMI)") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the boxplot
ggsave(sprintf("%s/Correlation_Boxplot_SMI_Level1.pdf", output_dir), width = 10, height = 6)
cat("✅ Boxplot saved:", sprintf("%s/Correlation_Boxplot_SMI_Level1.pdf", output_dir), "\n")
