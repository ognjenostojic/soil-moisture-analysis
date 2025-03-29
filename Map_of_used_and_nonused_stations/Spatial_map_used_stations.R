# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Set working directory (adjust as needed)
setwd("/home/xosto002/Beachelor_thesis")

# Load Europe map
europe_map <- ne_countries(scale = "medium", continent = "europe", returnclass = "sf")

# Load station datasets using base R
all_stations <- read.csv("Project/Lookup_tables/lookup_table_new.csv", stringsAsFactors = FALSE)
research_stations <- read.csv("Project/Lookup_tables/updated_fill_lookup_table_ismn_20240131.csv", stringsAsFactors = FALSE)

# Ensure lowercase for consistency
colnames(all_stations) <- tolower(colnames(all_stations))
colnames(research_stations) <- tolower(colnames(research_stations))

# Merge datasets to flag stations
station_data <- all_stations %>%
  mutate(used_in_research = ifelse(id %in% research_stations$id, "Used in Research", "Not Used"))

# Define colors
station_colors <- c("Not Used" = "#9E9E9E",  # medium gray
                    "Used in Research" = "#D62828")  # bright red

# Spatial plot with outlined legend
p <- ggplot() +
  geom_sf(data = europe_map, fill = "gray90", color = "black") +
  geom_point(data = station_data,
             aes(x = longitude, y = latitude, fill = used_in_research),
             shape = 21, color = "black", stroke = 0.3, size = 1.5, alpha = 0.9) +
  scale_fill_manual(values = station_colors) +
  labs(
    title = "Spatial Distribution of Stations",
    x = "Longitude", y = "Latitude",
    fill = "Station Status"
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(30, 75), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.1, 0.1),  # Moves legend to bottom-left corner
    legend.key.size = unit(0.5, "cm"),  # Smaller legend
    legend.text = element_text(size = 10),  # Smaller legend text
    legend.title = element_text(size = 11, face = "bold"),  # Smaller legend title
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Adds a white background with black border
    legend.box.background = element_rect(color = "black", size = 0.8)  # Adds a nice outer border
  )

# Save plot
ggsave("Spatial_Map_Stations.pdf", p, width = 10, height = 6)

cat("Saved: Spatial_Map_Stations.pdf\n")
