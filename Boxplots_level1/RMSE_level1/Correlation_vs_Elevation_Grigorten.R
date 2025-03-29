# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)
library(lfstat)  # For Grigorten function

# Set working directory and define output path
setwd("/home/xosto002/Beachelor_thesis")
output_dir <- "/home/xosto002/Beachelor_thesis/Project/Correlation_Elevation/Correlation_Elevation_Grigorten"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read lookup table containing all stations
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv", stringsAsFactors = FALSE)

# Define simulation file paths
simulation_dir_main <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022"
simulation_dir_1km <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER"

# Define Level 1 and resolutions
level <- "L01"
resolutions <- c("0p015625deg", "0p03125deg", "0p0625deg", "0p125deg", "0p25deg")

# Function to compute SMI using Grigorten transformation
calculate_smi_grigorten <- function(values) {
  if (length(values) < 5 || all(is.na(values))) return(rep(NA, length(values)))
  smi_values <- gringorten(values)
  return(smi_values)
}

# Function to read and compute SMI for STM (observation) data
read_and_clean_stm <- function(filename) {
  if (!file.exists(filename)) {
    cat(" Missing STM file:", filename, "\n")
    return(NULL)
  }
  
  lines <- readLines(filename)
  data <- read.table(text = lines[-1], sep = " ", fill = TRUE, header = FALSE)
  names(data) <- c("Date", "Time", "Value", "Quality", "Mode")
  data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y/%m/%d %H:%M", tz = "UTC")
  data$Value <- as.numeric(data$Value)
  
  # Remove NAs before calculating SMI
  data <- data %>% filter(!is.na(Value))
  
  # Apply Grigorten method to get SMI
  data$SMI <- calculate_smi_grigorten(data$Value)
  
  return(data[, c("Datetime", "SMI")])
}

# Function to read and compute SMI for NetCDF simulation data
read_nc_simulation <- function(file_path, resolution) {
  if (!file.exists(file_path)) {
    cat("Missing NetCDF file:", file_path, "\n")
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
    cat("No valid data in:", file_path, "\n")
    return(NULL)
  }
  
  # Compute SMI using Grigorten method
  smi_values <- calculate_smi_grigorten(sim_values)
  
  nc_close(nc_data)
  
  return(data.frame(Datetime = dates, SMI = smi_values, Resolution = resolution))
}

# Function to calculate Pearson correlation
calculate_correlation <- function(obs, sim) {
  common_dates <- intersect(obs$Datetime, sim$Datetime)
  obs_filtered <- obs %>% filter(Datetime %in% common_dates)
  sim_filtered <- sim %>% filter(Datetime %in% common_dates)
  
  if (nrow(obs_filtered) == 0 || nrow(sim_filtered) == 0) {
    return(NA)
  }
  
  return(cor(obs_filtered$SMI, sim_filtered$SMI, use = "complete.obs"))
}

# Dataframe to store correlation results
correlation_results <- data.frame()

# Loop through Level 1 stations
for (station_id in unique(lookup_table$ID)) {
  stm_file_path <- unique(lookup_table$Filename[lookup_table$ID == station_id])[1]
  elevation <- unique(lookup_table$Elevation[lookup_table$ID == station_id])[1]  # Get elevation
  
  # Read observation data
  obs_data <- read_and_clean_stm(stm_file_path)
  if (is.null(obs_data)) next
  
  for (resolution in resolutions) {
    sim_file_path <- ifelse(resolution == "0p015625deg",
                            sprintf("%s/SMvol_%s_%s_ID_%d_new.nc", simulation_dir_1km, level, resolution, station_id),  
                            sprintf("%s/SMvol_%s_%s_ID_%d.nc", simulation_dir_main, level, resolution, station_id))
    
    sim_data <- read_nc_simulation(sim_file_path, resolution)
    if (!is.null(sim_data)) {
      correlation_value <- calculate_correlation(obs_data, sim_data)
      correlation_results <- rbind(correlation_results, 
                                   data.frame(Station_ID = station_id, 
                                              Elevation = elevation, 
                                              Resolution = resolution, 
                                              Correlation = correlation_value))
    }
  }
}

# Save results to CSV
write.csv(correlation_results, sprintf("%s/Correlation_by_Elevation_Grigorten.csv", output_dir), row.names = FALSE)

# **Generate Scatter Plots: Correlation vs Elevation**
color_map <- c("0p015625deg" = "#E41A1C", "0p03125deg" = "#377EB8", "0p0625deg" = "#4DAF4A", "0p125deg" = "#FF7F00", "0p25deg" = "#984EA3")

plot <- ggplot(correlation_results, aes(x = Elevation, y = Correlation, color = Resolution)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", alpha = 0.6) +
  scale_color_manual(values = color_map) +
  labs(title = "Correlation by Elevation for Different Resolutions (Grigorten)",
       x = "Elevation (m)", 
       y = "Pearson Correlation Coefficient") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.85),  # Position legend at top-right corner
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(3, 3, 3, 3))

# Save plot
ggsave(sprintf("%s/Correlation_vs_Elevation_Grigorten.pdf", output_dir), plot, width = 10, height = 6)

cat("Correlation analysis (Grigorten) complete! Results saved in:", output_dir, "\n")
