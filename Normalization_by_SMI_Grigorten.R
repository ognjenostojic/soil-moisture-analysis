# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)
library(lfstat)  # For gringorten function

# Set working directory and define output path
setwd("/home/xosto002/Beachelor_thesis")
output_dir <- "/home/xosto002/Beachelor_thesis/Project/Normalization_SMIndex/Normalization_by_Grigorten/Grigorten_Normalization"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read lookup table containing all stations
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv", stringsAsFactors = FALSE)

# Define simulation file paths
simulation_dir_main <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022"
simulation_dir_1km <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER"

# Define Levels and resolutions
levels <- c("L01", "L02", "L03")
resolutions <- c("0p015625deg", "0p03125deg", "0p0625deg", "0p125deg", "0p25deg")

# Function to compute SMI using Gringorten plotting positions
calculate_smi_gringorten <- function(values) {
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
  
  # Apply Gringorten method to get SMI
  data$SMI <- calculate_smi_gringorten(data$Value)
  
  return(data[, c("Datetime", "SMI")])
}

# Function to determine the correct simulation file path based on level & resolution
get_simulation_file_path <- function(level, resolution, station_id) {
  if ((level == "L01" && resolution == "0p015625deg") ||
      (level == "L02" && resolution == "0p03125deg") ||
      (level == "L03" && resolution == "0p015625deg")) {
    return(sprintf("%s/SMvol_%s_%s_ID_%d_new.nc", simulation_dir_1km, level, resolution, station_id))
  } else {
    return(sprintf("%s/SMvol_%s_%s_ID_%d.nc", simulation_dir_main, level, resolution, station_id))
  }
}

# Function to read and compute SMI for NetCDF simulation data
read_nc_simulation <- function(file_path, resolution, obs_start, obs_end) {
  if (!file.exists(file_path)) {
    cat(" Missing NetCDF file:", file_path, "\n")
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
    cat(" No valid data in:", file_path, "\n")
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

# Generate plots for all stations across Levels L01, L02, L03
generate_plots <- function(station_id, stm_file_path, level) {
  obs_data <- read_and_clean_stm(stm_file_path)
  if (is.null(obs_data)) return(NULL)
  
  obs_start <- min(obs_data$Datetime, na.rm = TRUE)
  obs_end <- max(obs_data$Datetime, na.rm = TRUE)
  
  # Define color and line styles
  color_mapping <- c(
    "Observation" = "blue",
    "0p015625deg" = "#E41A1C",  
    "0p03125deg"  = "#377EB8",  
    "0p0625deg"   = "#4DAF4A",  
    "0p125deg"    = "#FF7F00",  
    "0p25deg"     = "#984EA3"   
  )
  
  line_types <- c(
    "Observation" = "solid",
    "0p015625deg" = "solid",
    "0p03125deg"  = "dashed",
    "0p0625deg"   = "dotted",
    "0p125deg"    = "dotdash",
    "0p25deg"     = "longdash"
  )
  
  all_sim_data <- data.frame()
  
  for (resolution in resolutions) {
    sim_file_path <- get_simulation_file_path(level, resolution, station_id)
    
    sim_data <- read_nc_simulation(sim_file_path, resolution, obs_start, obs_end)
    if (!is.null(sim_data)) {
      all_sim_data <- rbind(all_sim_data, sim_data)
    }
  }
  
  # Generate plot
  plot <- ggplot() +
    geom_line(data = obs_data, aes(x = Datetime, y = SMI, color = "Observation", linetype = "Observation"), size = 0.4) +
    labs(title = sprintf("Station ID: %d - %s", station_id, level), x = "Date", y = "Soil Moisture Index (SMI)") +
    theme_minimal() +
    ylim(0, 1) +
    facet_wrap(~Resolution, ncol = 2) +
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = line_types)
  
  if (nrow(all_sim_data) > 0) {
    plot <- plot +
      geom_line(data = all_sim_data, aes(x = Datetime, y = SMI, color = Resolution, linetype = Resolution), alpha = 0.6, size = 0.2)
  }
  
  # Save plot
  file_name <- sprintf("%s/plot_ID_%d_%s_combined.pdf", output_dir, station_id, level)
  ggsave(file_name, plot, width = 12, height = 8)
  cat(" Plot saved:", file_name, "\n")
}

# Process stations across Levels L01, L02, L03
for (level in levels) {
  for (i in 1:nrow(lookup_table)) {
    station_id <- lookup_table$ID[i]
    stm_file_path <- lookup_table$Filename[i]
    
    cat(" Processing Station:", station_id, "at", level, "\n")
    tryCatch({
      generate_plots(station_id, stm_file_path, level)
    }, error = function(e) {
      cat(" Error processing station:", station_id, "- Skipping...\n")
    })
  }
}

cat(" All plots for Levels L01, L02, L03 generated and saved in:", output_dir, "\n")
