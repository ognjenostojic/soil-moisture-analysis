# Load necessary libraries
library(ncdf4)
library(dplyr)
library(ggplot2)
library(lfstat)  # For gringorten function
library(lubridate)

# Define working directory and output paths
setwd("/home/xosto002/Beachelor_thesis")
output_dir <- "/home/xosto002/Beachelor_thesis/Project/Normalization_SMIndex/Normalization_by_Grigorten/Grigorten_Normalization_new_plots_DOY"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read lookup table containing all stations
lookup_table <- read.csv("/home/xosto002/Beachelor_thesis/Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv", stringsAsFactors = FALSE)

# Define simulation file paths
simulation_dir_main <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022"
simulation_dir_1km <- "/DATA/theses/ognjen/processed/mhm_timeseries_1998_2022_NEW_NOT_USE_TMP_FOLDER"

# Define Levels and Resolutions (ONLY LEVEL 1)
level <- "L01"
resolutions <- c("0p015625deg", "0p03125deg", "0p0625deg", "0p125deg", "0p25deg")

# Function to compute SMI using Gringorten for each DOY (Day-of-Year)
calculate_SMI_by_DOY <- function(SM, dates) {
  if (length(SM) != length(dates)) {
    cat("Error: Length mismatch between soil moisture and dates!\n")
    return(NULL)
  }
  
  # Create dataframe
  data <- data.frame(Date = dates, SM = SM)
  
  # Compute DOY (1-365)
  data$DOY <- yday(data$Date)
  
  # Apply Gringorten transformation per DOY
  data$SMI <- NA
  for (d in unique(data$DOY)) {
    indices <- which(data$DOY == d)
    if (length(indices) > 5) {  # Ensure enough data points
      data$SMI[indices] <- gringorten(data$SM[indices])
    }
  }
  
  return(data[, c("Date", "DOY", "SMI")])
}

# Function to determine correct simulation file path
get_simulation_file_path <- function(resolution, station_id) {
  if (resolution == "0p015625deg") {
    return(sprintf("%s/SMvol_%s_%s_ID_%d_new.nc", simulation_dir_1km, level, resolution, station_id))
  } else {
    return(sprintf("%s/SMvol_%s_%s_ID_%d.nc", simulation_dir_main, level, resolution, station_id))
  }
}

read_nc_simulation <- function(file_path, resolution, obs_start, obs_end) {
  if (!file.exists(file_path)) {
    cat("Missing NetCDF file:", file_path, "\n")
    return(NULL)
  }
  
  cat("Opening NetCDF file:", file_path, "\n")
  nc_data <- nc_open(file_path)
  
  # Extract soil moisture and time data
  SM <- ncvar_get(nc_data, "sm1vol")
  time_values <- ncvar_get(nc_data, "time")
  time_units <- ncatt_get(nc_data, "time", "units")$value
  
  # Convert time values to actual dates
  if (grepl("hours since", time_units)) {
    base_date <- as.POSIXct(substr(time_units, 13, 32), tz = "UTC")
    dates <- base_date + (time_values * 3600)
  } else {
    cat("Unknown time format:", time_units, "\n")
    return(NULL)
  }
  
  cat("Corrected Date Range:", min(dates, na.rm = TRUE), "to", max(dates, na.rm = TRUE), "\n")
  
  # Crop to observation period
  valid_indices <- which(dates >= obs_start & dates <= obs_end)
  
  if (length(valid_indices) == 0) {
    cat("No valid data within the observation period for", file_path, "\n")
    return(NULL)
  }
  
  SM <- SM[valid_indices]
  dates <- dates[valid_indices]
  
  # Debugging: Check if SM values exist
  cat(" Checking SM values: Min =", min(SM, na.rm = TRUE), "Max =", max(SM, na.rm = TRUE), "\n")
  
  # Compute SMI
  smi_data <- calculate_SMI_by_DOY(SM, dates)
  
  # Ensure SMI calculation worked
  if (is.null(smi_data)) {
    cat("Error: SMI calculation failed for", file_path, "\n")
    return(NULL)
  }
  
  smi_data$Resolution <- resolution
  nc_close(nc_data)
  
  return(smi_data)
}

generate_plots <- function(station_id, stm_file_path) {
  obs_data <- read_and_clean_stm(stm_file_path)
  if (is.null(obs_data)) return(NULL)
  
  obs_start <- min(obs_data$Datetime, na.rm = TRUE)
  obs_end <- max(obs_data$Datetime, na.rm = TRUE)
  
  all_sim_data <- data.frame()
  
  for (resolution in resolutions) {
    sim_file_path <- get_simulation_file_path(resolution, station_id)
    sim_data <- read_nc_simulation(sim_file_path, resolution, obs_start, obs_end)
    
    if (!is.null(sim_data)) {
      cat("Simulation data found for:", resolution, "at Station:", station_id, "\n")
      
      # Rename Date to Datetime for consistency with observations
      sim_data <- sim_data %>% rename(Datetime = Date)
      
      # Remove NA values
      sim_data <- sim_data %>% filter(!is.na(SMI))
      
      all_sim_data <- rbind(all_sim_data, sim_data)
    } else {
      cat("️ No data found for resolution:", resolution, "at Station:", station_id, "\n")
    }
  }
  
  if (nrow(all_sim_data) == 0) {
    cat(" No simulation data available for Station:", station_id, "\n")
    return(NULL)
  }
  
  print(head(all_sim_data))  # Debugging to check if simulation data is present
  
  # Define colors for better visualization
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
  
  # Generate the plot
  plot <- ggplot() +
    geom_line(data = obs_data, aes(x = Datetime, y = SMI, color = "Observation", linetype = "Observation"), size = 0.4) +
    labs(title = sprintf("SMI Comparison - Station ID: %d (L01)", station_id), x = "Date", y = "Soil Moisture Index (SMI)") +
    theme_minimal() +
    ylim(0, 1) +
    facet_wrap(~Resolution, ncol = 2) +  # Arrange plots in a 2-column grid
    scale_color_manual(values = color_mapping) +
    scale_linetype_manual(values = line_types)
  
  if (nrow(all_sim_data) > 0) {
    plot <- plot +
      geom_line(data = all_sim_data, aes(x = Datetime, y = SMI, color = Resolution, linetype = Resolution), alpha = 0.6, size = 0.2)
  }
  
  # Save plot
  file_name <- sprintf("%s/plot_ID_%d_L01_combined.pdf", output_dir, station_id)
  ggsave(file_name, plot, width = 12, height = 8)
  cat("Plot saved:", file_name, "\n")
}



# Process all Level 1 stations
for (i in 1:nrow(lookup_table)) {
  station_id <- lookup_table$ID[i]
  stm_file_path <- lookup_table$Filename[i]
  
  cat(" Processing Station:", station_id, "at Level 1\n")
  tryCatch({
    generate_plots(station_id, stm_file_path)
  }, error = function(e) {
    cat("Error processing station:", station_id, "- Skipping...\n")
  })
}

cat(" All plots for Level 1 generated and saved in:", output_dir, "\n")






