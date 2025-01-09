install.packages("dplyr")
install.packages("Kendall")
library(Kendall)
library(dplyr)

data <- read.csv("london_houses.csv", header = TRUE)

rows_with_na <- sum(apply(data, 1, function(row) any(is.na(row))))
rows_with_na

pearson_results <- list()
spearman_results <- list()
anova_results <- list()
kendall_results <- list()

for (col in names(data)) {
  if (col == "Price") next
  
  if (is.numeric(data[[col]])) {
    pearson_corr <- cor(data$Price, data[[col]], method = "pearson", use = "complete.obs")
    pearson_results[[col]] <- pearson_corr
    
    spearman_corr <- cor(data$Price, data[[col]], method = "spearman", use = "complete.obs")
    spearman_results[[col]] <- spearman_corr
  }
  
  if (is.factor(data[[col]]) || is.character(data[[col]])) {
    fit <- lm(Price ~ as.factor(data[[col]]), data = data)  
    anova_result <- anova(fit)  
    anova_results[[col]] <- anova_result

    kendall_corr <- Kendall(data$Price, as.factor(data[[col]]))
    kendall_results[[col]] <- kendall_corr
  }
}

print("Pearson's Correlation Coefficients:")
print(pearson_results)

print("Spearman's Correlation Coefficients:")
print(spearman_results)

print("ANOVA Results for Categorical Columns:")
print(anova_results)

print("Kendall's Tau Correlation for Categorical Columns:")
print(kendall_results)


