# MaxENT Line-Bar Plot Analysis for Millet Sources - Data from Supplementary Material - (High-Elevation Sites)

# Read CSV file
data <- read.csv("REDACTED")

# Import ggplot2 and reshape2 packages
library(ggplot2)
library(reshape2)

# Read CSV file excluding elevation column
data_filled <- read.csv("E:/all_csv/key sites.csv", header = TRUE, stringsAsFactors = FALSE)[,-2]

# Reshape filled data to long format
data_filled_long <- melt(data_filled, id.vars = "Site")

# Extract FM4000 and BM4000 data
data_FM_BM_4000 <- data_filled_long[data_filled_long$variable %in% c("FM4000", "BM4000"), ]
# Extract FM5000 and BM5000 data
data_FM_BM_5000 <- data_filled_long[data_filled_long$variable %in% c("FM5000", "BM5000"), ]

# Extract Elevation and Site data
elevation_site_data <- data.frame(Elevation = c(3920, 3680, 3650, 3160, 3140, 3100, 2867, 2848, 2840, 2630, 2527),
                                  Site = c("Khog Gzung", "Qugong", "Changguogou", "Liding", "Xiaoenda", "Karuo", "Kading", "Zongri", "Haxiu", "Liujiazhai", "Ajiacun"))

# Scale and transform Elevation column data to the range of 0 to 1
min_elevation <- min(elevation_site_data$Elevation)
max_elevation <- max(elevation_site_data$Elevation)
scaled_elevation <- (elevation_site_data$Elevation - min_elevation) / (max_elevation - min_elevation) * 0.5 + 1.2

# Update Elevation column in the data frame
elevation_site_data$Elevation <- scaled_elevation

# Extract FM4000 and BM4000 data, and add Elevation column
data_FM_BM_4000_with_elevation <- merge(data_FM_BM_4000, elevation_site_data, by = "Site", all.x = TRUE)

# Extract FM5000 and BM5000 data, and add Elevation column
data_FM_BM_5000_with_elevation <- merge(data_FM_BM_5000, elevation_site_data, by = "Site", all.x = TRUE)

# Plot bar chart (BM4000 and FM4000), and add line plot
p1 <- ggplot(data_FM_BM_4000_with_elevation, aes(x = Site, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.25) +  # Add borders to bar chart
  geom_line(aes(y = Elevation * max(data_FM_BM_4000$value), color = "Elevation"), group = 1, size = 1) +  # Customize line color and size, and add color mapping
  theme_minimal() +  # Apply minimal theme
  theme(axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom") +  # Place legend at the bottom
  labs(title = "BM4000 and FM4000 by Site", x = "Site", y = "Index", fill = "Variable") +  # Modify title and axis labels
  scale_fill_manual(values = c("FM4000" = "skyblue", "BM4000" = "#D77F94")) +  # Set legend colors
  scale_color_manual(values = c("Elevation" = "red")) +  # Set line color
  guides(fill = guide_legend(title = "Variable"), color = guide_legend(title = "Elevation")) +  # Modify legend titles
  ylim(0, 1.8) # Set y-axis range

# Plot bar chart (BM5000 and FM5000), and add line plot
p2 <- ggplot(data_FM_BM_5000_with_elevation, aes(x = Site, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.25) +  # Add borders to bar chart
  geom_line(aes(y = Elevation * max(data_FM_BM_5000$value), color = "Elevation"), group = 1, size = 1) +  # Customize line color and size, and add color mapping
  theme_minimal() +  # Apply minimal theme
  theme(axis.text.x = element_text(angle = 30, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom") +  # Place legend at the bottom
  labs(title = "BM5000 and FM5000 by Site", x = "Site", y = "Index", fill = "Variable") +  # Modify title and axis labels
  scale_fill_manual(values = c("FM5000" = "skyblue", "BM5000" = "#D77F94")) +  # Set legend colors
  scale_color_manual(values = c("Elevation" = "red")) +  # Set line color
  guides(fill = guide_legend(title = "Variable"), color = guide_legend(title = "Elevation")) +  # Modify legend titles
  ylim(0, 1.8) # Set y-axis range

# Output the two plots
print(p1)
print(p2)

