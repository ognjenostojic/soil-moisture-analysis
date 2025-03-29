# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the RMSE results from CSV
rmse_results <- read.csv(sprintf("%s/RMSE_results_Land_Cover.csv", output_dir))

# Ensure correct ordering of resolutions
rmse_results$Resolution <- factor(rmse_results$Resolution, levels = c("1km", "3km", "5km", "11km", "22km"))

# Filter data for Cropland and Forest
cropland_results <- rmse_results %>% filter(grepl("Cropland", Land_Cover, ignore.case = TRUE))
forest_results <- rmse_results %>% filter(grepl("Tree cover|Forest", Land_Cover, ignore.case = TRUE))

# Define colors for the plots
color_map <- c("1km" = "#E41A1C", "3km" = "#377EB8", "5km" = "#4DAF4A", "11km" = "#FF7F00", "22km" = "#984EA3")

# Generate Cropland RMSE Boxplot
if (nrow(cropland_results) > 0) {
  p_cropland <- ggplot(cropland_results, aes(x = Resolution, y = RMSE, fill = Resolution)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
    scale_fill_manual(values = color_map) +
    labs(title = "RMSE for Cropland Across Resolutions (Level 1)", x = "Resolution", y = "RMSE") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(sprintf("%s/RMSE_Cropland_Level1.pdf", output_dir), p_cropland, width = 10, height = 6)
  cat("✅ Cropland RMSE boxplot saved.\n")
}

# Generate Forest RMSE Boxplot
if (nrow(forest_results) > 0) {
  p_forest <- ggplot(forest_results, aes(x = Resolution, y = RMSE, fill = Resolution)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
    scale_fill_manual(values = color_map) +
    labs(title = "RMSE for Forest Across Resolutions (Level 1)", x = "Resolution", y = "RMSE") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(sprintf("%s/RMSE_Forest_Level1.pdf", output_dir), p_forest, width = 10, height = 6)
  cat("✅ Forest RMSE boxplot saved.\n")
}

cat("✅ Boxplot generation complete!\n")
