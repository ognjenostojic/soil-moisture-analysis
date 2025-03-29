# Load necessary libraries
library(ncdf4)
library(ggplot2)
library(RColorBrewer)

# Set working directory
setwd("/DATA/theses/ognjen/raw")  

# Define output directory
output_dir <- "/home/xosto002/Beachelor_thesis/Project/SpatialMaps_Resolutions"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Find the NetCDF file for 1km resolution (0p015625deg)
files <- list.files(pattern = "SMvol_L01_0p015625deg\\.nc", full.names = TRUE)

# Check if the file exists
if (length(files) == 0) {
  stop("⚠️ No file found for 1km resolution.")
}

file_path <- files[1]  # Use the first match

cat("📂 Processing:", file_path, "\n")

# Open NetCDF file
nc_data <- nc_open(file_path)

# Extract longitude, latitude, and soil moisture variables
lon <- ncvar_get(nc_data, "lon")  # Check if correct variable name
lat <- ncvar_get(nc_data, "lat")  # Check if correct variable name
soil_moisture <- ncvar_get(nc_data, "sm1vol")  # Adjust variable name if needed

# Select the latest time step if the data is 3D
if (length(dim(soil_moisture)) == 3) {
  time_dim <- dim(soil_moisture)[3]
  soil_moisture <- soil_moisture[,,time_dim]  # Last time step
}

# Close NetCDF file
nc_close(nc_data)

# Create dataframe for plotting
df <- expand.grid(lon = lon, lat = lat)
df$soil_moisture <- as.vector(soil_moisture)

# Remove NA values
df <- na.omit(df)

# Custom color palette
num_colors <- 150
original_palette <- brewer.pal(9, "Spectral")
intense_palette <- colorRampPalette(original_palette)(num_colors)
reversed_palette <- rev(intense_palette)

# Plotting
p <- ggplot(data = df, aes(x = lon, y = lat, fill = soil_moisture)) +
  geom_tile() +
  scale_fill_gradientn(colors = reversed_palette) + 
  coord_fixed() +
  labs(fill = "Soil Moisture", x = "Longitude", y = "Latitude",
       title = "Soil Moisture Distribution - 1km (Level 1)") +
  theme_minimal()

# Define output file path
output_file <- sprintf("%s/Soil_Moisture_Map_L1_1km.pdf", output_dir)

# Save the plot
ggsave(output_file, p, width = 10, height = 6)

cat("✅ Saved:", output_file, "\n")
