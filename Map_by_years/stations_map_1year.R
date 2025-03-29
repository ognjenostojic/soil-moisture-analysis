library(ggplot2)
library(dplyr)
library(maps)

setwd("/home/xosto002/Beachelor_thesis/") 
lookup_table <- read.csv("lookup_table_new.csv")

# Filter data for valid latitude and longitude values
stations_data <- lookup_table %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  select(Longitude, Latitude)


world_map <- map_data("world")
europe_map <- world_map %>%
  filter(region %in% c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France","England", "Germany", "Greece", "Hungary", 
    "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "United Kingdom", "Norway", "Switzerland", "Russia",
    "Bosnia and Herzegovina", "Serbia", "North Macedonia", "Albania", 
    "Moldova", "Ukraine", "Belarus", "Kosovo", "UK"
  ))

# Plotting
map_plot <- ggplot() +
  # Add the Europe map as polygons
  geom_polygon(data = europe_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white") +
  # Add station points
  geom_point(data = stations_data, aes(x = Longitude, y = Latitude), 
             color = "red", size = 0.08) +  # Adjust point size as needed
  coord_quickmap(xlim = c(-30, 50), ylim = c(35, 72)) +  # Focus on Europe
  labs(x = "Longitude", y = "Latitude", 
       title = "Spatial Distribution of Stations with Data of at least 1 Year") +
  theme_minimal()

# Print the map to the screen
print(map_plot)

# Save the map as a PDF
ggsave("/home/xosto002/Beachelor_thesis/spatial_map/stations_map_1year.pdf", plot = map_plot, width = 10, height = 6)
