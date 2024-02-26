# IDW Interpolation Accuracy Test.
# Install and load the ggplot2 package
library(ggplot2)

# Get all CSV files in the specified folder
folder_path <- "REDACTED"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list for plots
plots_list <- list()

# Loop through each file
for (file_path in file_list) {
  # Read the data
  data <- read.csv(file_path)
  
  # Calculate R² and RMSE
  r_squared <- cor(data$Measured, data$Predicted)^2
  rmse <- sqrt(mean(data$Error^2))
  
  # Plot RMSE and R²
  combined_plot <- ggplot(data, aes(x = Measured, y = Predicted)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("RMSE and R² Plot -", basename(file_path)),
         x = "Measured",
         y = "Predicted") +
    annotate("text", x = max(data$Measured), y = max(data$Predicted),
             label = paste("R² =", round(r_squared, 3)),
             hjust = 1, vjust = 8, color = "blue") +
    annotate("text", x = max(data$Measured), y = max(data$Predicted),
             label = paste("RMSE =", round(rmse, 3)),
             hjust = 1, vjust = 15, color = "red")
  
  # Add the plot to the list
  plots_list[[file_path]] <- combined_plot
}

# Display the plots
for (plot in plots_list) {
  print(plot)
}
