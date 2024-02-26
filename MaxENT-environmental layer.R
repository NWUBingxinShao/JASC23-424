# MaxENT Model Environmental Layer Selection Process Code
# Including Statistical Analysis of MaxENT Results (Multiple Linear Regression Model),(Supplementary-AUC)
# Set folder path
folder_path <- "REDACTED"

# Get file names of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store data frames
data_frames_list <- list()

# Loop through each CSV file and create a corresponding data frame
for (file_path in csv_files) {
  # Extract characters separated by underscores from the file name
  file_name <- tools::file_path_sans_ext(basename(file_path))
  file_parts <- unlist(strsplit(file_name, "_"))
  
  # Create a data frame and name it
  df <- read.csv(file_path)
  df_name <- paste(file_parts, collapse = "_")
  assign(df_name, df)
  
  # Add the data frame to the list
  data_frames_list[[df_name]] <- df
}

# Create an empty data frame to store the values of the Training.AUC column
result_df <- data.frame()

# Initialize the result data frame
result_df <- data.frame(DataFrame = character(), Training.AUC = numeric(), Test.AUC = numeric())

# Loop through each data frame
for (df_name in names(data_frames_list)) {
  # Extract the values of the Training.AUC column and Test.AUC column
  training_AUC_values <- data_frames_list[[df_name]]$Training.AUC
  test_AUC_values <- data_frames_list[[df_name]]$Test.AUC
  
  # Create a temporary data frame containing the data frame name, Training.AUC, and Test.AUC values
  temp_df <- data.frame(DataFrame = df_name, Training.AUC = training_AUC_values, Test.AUC = test_AUC_values)
  
  # Handle duplicate names
  temp_df$DataFrame <- ave(temp_df$DataFrame, temp_df$DataFrame, FUN = function(x) paste(x, seq_along(x), sep = "_AUC"))
  
  # Add the temporary data frame to the result data frame
  result_df <- rbind(result_df, temp_df)
}

# Load the ggplot2 package
library(ggplot2)
library(MASS)

# Extract crop type keywords
result_df$Crop_Type <- sub("([^_]+)_.*", "\\1", result_df$DataFrame)

# Extract keywords
result_df$Sampling_Method <- sub(".*_(BS|SS|CV).*", "\\1", result_df$DataFrame)

# Extract environmental variable feature keywords
result_df$Environment <- sub(".*_(LH|MH|MH30s|NOW).*", "\\1", result_df$DataFrame)

# Extract crop type, sampling method, and environmental variable feature from the result data frame as independent variables
Crop_Type <- as.factor(result_df$Crop_Type)
Sampling_Method <- as.factor(result_df$Sampling_Method)
Environment <- as.factor(result_df$Environment)

# Displaying model coefficients, confidence intervals, and significance test results
summary(model)
confint(model)

# Checking if residuals follow a normal distribution
shapiro.test(model$residuals)
hist(model$residuals, main="Histogram of Residuals", xlab="Residuals")

# Using Training.AUC as the dependent variable
Training_AUC <- result_df$Training.AUC

# Building a multiple linear regression model with all possible interactions
model <- lm(Training_AUC ~ Crop_Type * Sampling_Method * Environment, data = result_df)

# Calculating predicted values for each factor combination using the predict function from the ggplot2 package
result_df$Predicted_AUC <- predict(model, newdata = result_df)

# Specifying the display order of Environment
desired_order <- c("LH", "MH30s", "MH", "NOW")

# Refactoring Environment and specifying the order
result_df$Environment <- factor(result_df$Environment, levels = desired_order)

# Plotting the interaction plot
ggplot(data = result_df, aes(x = Crop_Type, y = Predicted_AUC, group = interaction(Sampling_Method, Environment), color = Sampling_Method, linetype = Environment)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Predicted_AUC - 1.96 * sd(Predicted_AUC), ymax = Predicted_AUC + 1.96 * sd(Predicted_AUC)), width = 0.2, size = 0.8) +
  scale_x_discrete(limits = c("M", "BM", "FM")) +
  labs(x = "Crop Type", y = "Predicted Training AUC", color = "Sampling Method", linetype = "Environment") +
  ggtitle("Interaction Plot of Crop Type and Sampling Method") +
  facet_wrap(~ Environment)


# Plotting a density plot to display the distribution of the dependent variable across different levels of treatments
library(ggplot2)

# Defining the color palette
color_palette <- c("#8FD2BD","#3983B1","#1891A1","#27A47C","#94C7DF","#D73026")

# Plotting using ggplot2 and adding a color palette
ggplot(result_df, aes(x = Training_AUC, fill = factor(Environment))) +
  geom_density(alpha = 1) +
  scale_fill_manual(values = color_palette) +
  labs(x = "Training AUC Value", y = "Density", fill = "Environment", title = "Density Plot of Training AUC Value by Environment")


# Using Training.AUC as the dependent variable
Training_AUC <- result_df$Training.AUC

# Building a linear regression model with only environment
model_2 <- glm(Training_AUC ~ Environment, data = result_df)

# Calculating predicted values for each environment using the predict function from the ggplot2 package
result_df$Predicted_AUC <- predict(model_2, newdata = result_df)

# Displaying model coefficients, confidence intervals, and significance test results
summary(model_2)
confint(model_2)

# Q-Q plot
# Plotting using ggpubr package
library(ggpubr)
# Plotting using ggqqplot function
ggqqplot(model_2$residuals, conf.int = TRUE) +
  theme_minimal() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_blank()
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 4, keywidth = 1, keyheight = 1))



