install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape")

library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("london_houses.csv", header = TRUE)

rows_with_na <- sum(apply(data, 1, function(row) any(is.na(row))))
rows_with_na

# Loop through all numeric columns
numeric_cols <- sapply(data, is.numeric)  # Identify numeric columns
for (col in names(data)[numeric_cols]) {
  hist_data <- hist(data[[col]], plot = FALSE)
  plot(hist_data$mids, hist_data$density, type = "l", 
       main = paste("Line Histogram of", col), 
       xlab = col, ylab = "Density", col = "darkgreen", lwd = 2)
}

# Loop through all numeric columns
numeric_cols <- sapply(data, is.numeric) # Identify numeric columns
for (col in names(data)[numeric_cols]) {
  hist(data[[col]], main = paste("Histogram of", col), xlab = col, col = "lightgreen")
}

# Loop through all numeric columns
numeric_cols <- sapply(data, is.numeric)  # Identify numeric columns
for (col in names(data)[numeric_cols]) {
  boxplot(data[[col]], 
          main = paste("Box Plot of", col), 
          ylab = col, col = "orange")
}

categorical_encoded <- data
Price <- cut(categorical_encoded$Price, breaks = 3)
levels(Price) 
levels(Price) <- c("Low", "Medium", "High")
categorical_encoded$Price_labels <- Price
table(categorical_encoded$Price_labels)

color_mapping <- c("Low" = "blue", "Medium" = "green", "High" = "red")
colors <- color_mapping[categorical_encoded$Price_labels]

# Adjust margins to make space for the legend
par(mar = c(12, 4, 8, 8))  # Increase the right margin (4th value)

# Scatterplot matrix
pairs(categorical_encoded[, sapply(categorical_encoded, is.numeric)], 
      col = colors, 
      pch = 19, 
      main = "Scatterplot Matrix")

numeric_columns <- names(categorical_encoded)[sapply(categorical_encoded, is.numeric)]
for (col in numeric_columns) {
  if (col == "Price") next
  # Generate the plot
  plot <- ggplot(categorical_encoded, aes_string(x = "Price_labels", y = col)) +
    geom_violin(fill = "skyblue", color = "black") +  # Violin plot
    geom_boxplot(width = 0.1, outlier.color = "red") +  # Boxplot overlay
    labs(title = paste("Violin Plot of", col, "by Price Labels"), 
         x = "Price Labels", 
         y = col) +
    theme_minimal()
  
  # Print the plot
  print(plot)
}

numeric_cols <- names(categorical_encoded)[sapply(categorical_encoded, is.numeric)]

# Loop through all unique combinations of numeric columns
for (i in 1:(length(numeric_cols) - 1)) {
  for (j in (i + 1):length(numeric_cols)) {
    # Get the combination of columns
    combo <- c(numeric_cols[i], numeric_cols[j])
    
    # Create the plot data for the first 50 rows of the current combination
    plot_data <- data.frame(
      x = categorical_encoded[[combo[1]]][1:20],
      y = categorical_encoded[[combo[2]]][1:20]
    )
    
    # Create the line graph
    plot <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_line(size = 1) +  # Connect points with lines
      geom_point(size = 3) +  # Add points to the plot
      labs(x = combo[1], y = combo[2], title = paste("Line Graph for", paste(combo, collapse = " vs. "))) +
      theme_minimal()
    
    # Print the plot
    print(plot)
  }
}
