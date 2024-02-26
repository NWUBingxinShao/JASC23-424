# Climate Data Calibration
# Quantification of temperature and precipitation changes in northern China during the “5000-year” Chinese History

# Read Zhang et al. (2021) data
Zhang2021 <- read.csv("REDACTED")

# Extract data from 1961 to 2021
instrumental_data <- data_group1[data_group1$Age >= 1961 & data_group1$Age <= 2021, ]

# Calculate standard deviation
calibration_sd <- sd(instrumental_data$Anomaly)

# Retrieve data from: http://www.sciencemag.org/content/suppl/2013/03/07/339.6124.1198.DC1/Marcott.SM.database.S1.xlsx
# Read temperature reconstruction data from Marcott et al. (2013), ensuring column names are consistent with the original code
marcott2013 <- read.csv("REDACTED")

# Filter valid data
marcott2013 <- marcott2013[(marcott2013$Age <= 6000) & (!is.na(marcott2013$Global)),]

# Calculate Z-scores
marcott2013$Z_Lower <- (marcott2013$Global - marcott2013$Uncertainty) / calibration_sd
marcott2013$Z <- marcott2013$Global / calibration_sd
marcott2013$Z_Upper <- (marcott2013$Global + marcott2013$Uncertainty) / calibration_sd

# Load ggplot2 package
library(ggplot2)

# Set theme
theme_set(theme_bw())

# Plot
ggplot(marcott2013, aes(x = Age, y = Global)) +
  geom_line(color = "red", size = 1.0) +  # Set line color and thickness
  geom_ribbon(aes(ymin = Z_Lower * calibration_sd, ymax = Z_Upper * calibration_sd), alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.03, se = FALSE, color = "blue", size = 1.0) +  # Add a loess fit
  scale_y_continuous(limits = c(0, 10)) +  # Set y-axis limits
  coord_cartesian(xlim = c(0, max(marcott2013$Age))) +  # Set x-axis range from 0 to the maximum value
  labs(x = "Age (cal yr BP)", y = "Anomaly (℃)", title = "Global Temperature Reconstruction") +
  scale_x_reverse() +  # Reverse x-axis
  # Add vertical line
  geom_vline(xintercept = 100, linetype = "dashed", color = "black", size = 0.5) +
  # Add text
  annotate("text", x = 100, y = 0.5, label = "Age = 100", hjust = 0.1, color = "black")
