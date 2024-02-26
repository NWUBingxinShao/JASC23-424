# Environmental Layer Correlation Analysis - Data from GIS Value Extraction - Data Stored in (Supplementary-result)
# Load the corrplot package
library(corrplot)
# Load the RColorBrewer package
library(RColorBrewer)

# Read and process each CSV file
file_paths <- c("REDACTED", "REDACTED", "REDACTED", "REDACTED", "REDACTED")

cor_matrices <- list()

for (file_path in file_paths) {
  # Read the CSV file
  millet_data <- read.csv(file_path)
  
  # Select columns to keep
  selected_columns <- c("bi1", "bi2", "bi3", "bi4", "bi5", 
                        "bi6", "bi7", "bi8", "bi9", "bi10", 
                        "bi11", "bi12", "bi13", "bi14", "bi15", 
                        "bi16", "bi17", "bi18", "bi19")
  
  # Keep only the specified columns
  millet_data_selected <- millet_data[, selected_columns]
  
  # Compute Pearson correlation coefficients between specified columns
  cor_matrix <- cor(millet_data_selected, method = "pearson")
  
  # Save the computed correlation matrix to the list
  cor_matrices[[file_path]] <- cor_matrix
}

# Create a gradient color scheme using the colorRampPalette function
col <- colorRampPalette(c("skyblue", "white", "red"))(100)

# Plot the heatmap, adjust the value of tl.cex
corrplot(cor_matrices[[1]], method = "color", type = "lower", order='AOE',
         tl.col = "black", tl.srt = 0, addCoef.col = "black", 
         number.cex = 0.8, number.digits = 2, cl.cex = 0.8, 
         title = "Millet Data Correlation Matrix - LH", col = col, tl.cex = 0.8)

# Plot heatmaps for other correlation matrices
# Repeat this for each file to plot a heatmap for each one
corrplot(cor_matrices[[2]], method = "color", type = "lower", order='AOE',
         tl.col = "black", tl.srt = 0, addCoef.col = "black", 
         number.cex = 0.8, number.digits = 2, cl.cex = 0.8, 
         title = "Millet Data Correlation Matrix - MH", col = col, tl.cex = 0.8)
corrplot(cor_matrices[[3]], method = "color", type = "lower", order='AOE',
         tl.col = "black", tl.srt = 0, addCoef.col = "black", 
         number.cex = 0.8, number.digits = 2, cl.cex = 0.8, 
         title = "Millet Data Correlation Matrix - MH30s", col = col, tl.cex = 0.8)
corrplot(cor_matrices[[4]], method = "color", type = "lower", order='AOE',
         tl.col = "black", tl.srt = 0, addCoef.col = "black", 
         number.cex = 0.8, number.digits = 2, cl.cex = 0.8, 
         title = "Millet Data Correlation Matrix - NOW", col = col, tl.cex = 0.8)
corrplot(cor_matrices[[5]], method = "color", type = "lower", order='AOE',
         tl.col = "black", tl.srt = 0, addCoef.col = "black", 
         number.cex = 0.8, number.digits = 2, cl.cex = 0.8, 
         title = "Millet Data Correlation Matrix - NOWd1", col = col, tl.cex = 0.8)

# Calculate Euclidean distances
euclidean_distances <- matrix(0, nrow = length(file_paths), ncol = length(file_paths))
for (i in 1:length(file_paths)) {
  for (j in 1:length(file_paths)) {
    euclidean_distances[i, j] <- sqrt(sum((cor_matrices[[i]] - cor_matrices[[j]])^2))
  }
}

# Calculate correlation coefficients
correlation_coefficients <- matrix(0, nrow = length(file_paths), ncol = length(file_paths))
for (i in 1:length(file_paths)) {
  for (j in 1:length(file_paths)) {
    correlation_coefficients[i, j] <- cor(as.vector(cor_matrices[[i]]), as.vector(cor_matrices[[j]]), method = "pearson")
  }
}

# Print the Euclidean distance matrix
print("Euclidean Distances:")
print(euclidean_distances)

# Print correlation coefficient matrices
print("Correlation Coefficients:")
print(correlation_coefficients)
