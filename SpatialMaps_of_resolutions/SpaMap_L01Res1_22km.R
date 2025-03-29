# Load necessary libraries
library(ncdf4)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Set working directory
setwd("C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis/Project/SpatialMaps_Resolutions")

# Define output directory
output_dir <- "C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis/Project/SpatialMaps_Resolutions"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to read and extract soil moisture data from NetCDF file
extract_soil_moisture <- function(file_path) {
  nc_data <- nc_open(file_path)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  soil_moisture <- ncvar_get(nc_data, "sm1vol")
  
  # Select the latest time step if the data is 3D
  if (length(dim(soil_moisture)) == 3) {
    time_dim <- dim(soil_moisture)[3]
    soil_moisture <- soil_moisture[,,time_dim] 
  }
  
  nc_close(nc_data)
  
  df <- expand.grid(lon = lon, lat = lat)
  df$soil_moisture <- as.vector(soil_moisture)
  df <- na.omit(df)
  
  return(df)
}

# Extract data for both resolutions
file_1km <- "C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis/Project/SpatialMaps_Resolutions/1km_22km_spa_map/SMvol_L01_0p015625deg_lasttimestep.nc"
file_22km <- "C:/Users/Ognjen/Desktop/Beachelor thesis/Beachelor_thesis/Beachelor_thesis/Project/SpatialMaps_Resolutions/1km_22km_spa_map/SMvol_L01_0p25deg_lasttimestep.nc"

df_1km <- extract_soil_moisture(file_1km)
df_22km <- extract_soil_moisture(file_22km)

# Custom color palette
num_colors <- 150
original_palette <- brewer.pal(9, "Spectral")
intense_palette <- colorRampPalette(original_palette)(num_colors)
reversed_palette <- rev(intense_palette)

# Plotting for 1 km resolution
p1 <- ggplot(data = df_1km, aes(x = lon, y = lat, fill = soil_moisture)) +
  geom_tile() +
  scale_fill_gradientn(colors = reversed_palette) + 
  coord_fixed() +
  labs(fill = "Soil Moisture", x = "Longitude", y = "Latitude", title = "a) Soil Moisture Distribution - 1 km (Level 1)") +
  theme_minimal()

# Plotting for 22 km resolution
p2 <- ggplot(data = df_22km, aes(x = lon, y = lat, fill = soil_moisture)) +
  geom_tile() +
  scale_fill_gradientn(colors = reversed_palette) + 
  coord_fixed() +
  labs(fill = "Soil Moisture", x = "Longitude", y = "Latitude", title = "b) Soil Moisture Distribution - 22 km (Level 1)") +
  theme_minimal()

# Arrange both plots into one figure
combined_plot <- grid.arrange(p1, p2, ncol = 1)

# Save the combined plot
output_file <- sprintf("%s/Soil_Moisture_Map_L1_combined.pdf", output_dir)
ggsave(output_file, combined_plot, width = 15, height = 6)

cat("Combined map saved:", output_file, "\n")
