install.packages("dplyr")
install.packages("Kendall")
library(Kendall)
library(dplyr)

# Read data and load necessary libraries
data <- read.csv("london_houses.csv", header = TRUE)

rows_with_na <- sum(apply(data, 1, function(row) any(is.na(row))))
rows_with_na

# Ensure that the "Price" column is numeric (remove any non-numeric characters if necessary)
data$Price <- as.numeric(gsub("[^0-9]", "", data$Price))

# Initialize lists to store results
pearson_results <- list()
spearman_results <- list()
anova_results <- list()
kendall_results <- list()

# Loop through all columns
for (col in names(data)) {
  if (col == "Price") next  # Skip the target column
  
  # Check if the column is numeric
  if (is.numeric(data[[col]])) {
    # Calculate Pearson's correlation coefficient
    pearson_corr <- cor(data$Price, data[[col]], method = "pearson", use = "complete.obs")
    pearson_results[[col]] <- pearson_corr
    
    # Calculate Spearman's correlation coefficient
    spearman_corr <- cor(data$Price, data[[col]], method = "spearman", use = "complete.obs")
    spearman_results[[col]] <- spearman_corr
  }
  
  if (is.factor(data[[col]]) || is.character(data[[col]])) {
    # Perform ANOVA for categorical columns
    fit <- lm(Price ~ as.factor(data[[col]]), data = data)  # Create linear model
    anova_result <- anova(fit)  # Perform ANOVA
    anova_results[[col]] <- anova_result
    
    # Perform Kendall's Tau correlation using DescTools
    kendall_corr <- Kendall(data$Price, as.factor(data[[col]]))
    kendall_results[[col]] <- kendall_corr
  }
}

# Print results
print("Pearson's Correlation Coefficients:")
print(pearson_results)

print("Spearman's Correlation Coefficients:")
print(spearman_results)

print("ANOVA Results for Categorical Columns:")
print(anova_results)

print("Kendall's Tau Correlation for Categorical Columns:")
print(kendall_results)


