# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)
library(lfstat)

# Set working directory and define output path
setwd("C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis")
output_dir <- "C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis/Project/BoxPlots/Grigorten_RMSE_L02"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read lookup table and filter by Avg_Depth
lookup_table <- read.csv("C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/lookup_table_with_filenames_neeeeeeew.csv", stringsAsFactors = FALSE)
lookup_table <- lookup_table %>% filter(Avg_Depth >= 0.05 & Avg_Depth <= 0.15)

# Define parent directory for all resolution folders
sim_base_dir <- "C:/Users/Ognjen/Desktop/theses/ognjen/processed"

# Map folder names to resolution labels
res_folders <- list(
  "0p015625deg" = "mhm_0p015625deg_L01_L02_L03",
  "0p03125deg"  = "mhm_0p03125deg_L01_L02_L03",  # special for L02
  "0p0625deg"   = "mhm_0p0625deg_L01_L02_L03",
  "0p125deg"    = "mhm_0p125deg_L01_L02_L03",
  "0p25deg"     = "mhm_0p25deg_L01_L02_L03"
)

resolutions <- c("0p015625deg" = "1km", 
                 "0p03125deg"  = "3km", 
                 "0p0625deg"   = "5km", 
                 "0p125deg"    = "11km", 
                 "0p25deg"     = "22km")

# Gringorten SMI
calculate_smi_gringorten <- function(values) {
  if (length(values) < 5 || all(is.na(values))) return(rep(NA, length(values)))
  gringorten(values)
}

# STM Reader
read_and_clean_stm <- function(filename) {
  if (!file.exists(filename)) {
    cat("Missing STM file:", filename, "\n")
    return(NULL)
  }
  lines <- readLines(filename)
  data <- read.table(text = lines[-1], sep = " ", fill = TRUE, header = FALSE)
  names(data) <- c("Date", "Time", "Value", "Quality", "Mode")
  data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y/%m/%d %H:%M", tz = "UTC")
  data$Value <- as.numeric(data$Value)
  data <- data %>% filter(!is.na(Value))
  data$SMI <- calculate_smi_gringorten(data$Value)
  return(data[, c("Datetime", "SMI")])
}

# NetCDF Reader
read_nc_simulation <- function(file_path, resolution, obs_start, obs_end) {
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
  sim_values <- (sim_values * scale_factor) + add_offset
  sim_values[sim_values == fill_value] <- NA
  
  sim_data <- data.frame(Datetime = dates, Simulated_Value = sim_values, Resolution = resolution)
  sim_data <- sim_data %>% filter(Datetime >= obs_start & Datetime <= obs_end)
  sim_data$SMI <- calculate_smi_gringorten(sim_data$Simulated_Value)
  nc_close(nc_data)
  return(sim_data)
}

# RMSE Calculator
calculate_rmse <- function(obs, sim) {
  common_dates <- intersect(obs$Datetime, sim$Datetime)
  obs_filtered <- obs %>% filter(Datetime %in% common_dates)
  sim_filtered <- sim %>% filter(Datetime %in% common_dates)
  if (nrow(obs_filtered) == 0 || nrow(sim_filtered) == 0) return(NA)
  sqrt(mean((obs_filtered$SMI - sim_filtered$SMI)^2, na.rm = TRUE))
}

# Store results
rmse_results <- data.frame()

# Loop through stations
for (station_id in unique(lookup_table$ID)) {
  stm_file_path <- unique(lookup_table$Filename[lookup_table$ID == station_id])[1]
  obs_data <- read_and_clean_stm(stm_file_path)
  if (is.null(obs_data)) next
  
  obs_start <- min(obs_data$Datetime, na.rm = TRUE)
  obs_end <- max(obs_data$Datetime, na.rm = TRUE)
  
  for (res_code in names(res_folders)) {
    folder_path <- file.path(sim_base_dir, res_folders[[res_code]])
    
    # Handle resolution-specific path and filename logic
    if (res_code == "0p03125deg") {
      # Special case: 3km files for L02 from different folder and with "_new.nc"
      folder_path <- "C:/Users/Ognjen/Desktop/theses/ognjen/processed/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER/OLDA/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER"
      filename_pattern <- sprintf("SMvol_L02_%s_ID_%d_new.nc", res_code, station_id)
    } else {
      folder_path <- file.path(sim_base_dir, res_folders[[res_code]])
      filename_pattern <- sprintf("SMvol_L02_%s_ID_%d.nc", res_code, station_id)
    }
    
    
    sim_file_path <- file.path(folder_path, filename_pattern)
    
    sim_data <- read_nc_simulation(sim_file_path, res_code, obs_start, obs_end)
    
    if (!is.null(sim_data)) {
      rmse_value <- calculate_rmse(obs_data, sim_data)
      rmse_results <- rbind(rmse_results, 
                            data.frame(Station_ID = station_id, 
                                       Resolution = resolutions[res_code], 
                                       RMSE = rmse_value))
    }
  }
}

# Save RMSE results
write.csv(rmse_results, sprintf("%s/RMSE_results_SMI.csv", output_dir), row.names = FALSE)

# Plot Boxplot
resolution_order <- c("1km", "3km", "5km", "11km", "22km")

ggplot(rmse_results, aes(x = factor(Resolution, levels = resolution_order), y = RMSE, fill = Resolution)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
  scale_fill_manual(values = c("1km" = "#E41A1C", "3km" = "#377EB8", "5km" = "#4DAF4A", "11km" = "#FF7F00", "22km" = "#984EA3")) +
  labs(title = "RMSE Boxplot for Different Resolutions (Level 2) - SMI", x = "Resolution", y = "RMSE (SMI)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(sprintf("%s/RMSE_Boxplot_SMI_Level2.pdf", output_dir), width = 10, height = 6)
cat("Boxplot saved:", sprintf("%s/RMSE_Boxplot_SMI_Level2.pdf", output_dir), "\n")

# Count number of unique stations used
num_used_stations <- length(unique(rmse_results$Station_ID))
cat(" Number of stations used in RMSE calculation:", num_used_stations, "\n")
