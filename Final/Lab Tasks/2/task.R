install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape")

library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("london_houses.csv", header = TRUE)

rows_with_na <- sum(apply(data, 1, function(row) any(is.na(row))))
rows_with_na

numeric_cols <- sapply(data, is.numeric)  
for (col in names(data)[numeric_cols]) {
  hist_data <- hist(data[[col]], plot = FALSE)
  plot(hist_data$mids, hist_data$density, type = "l", 
       main = paste("Line Histogram of", col), 
       xlab = col, ylab = "Density", col = "darkgreen", lwd = 2)
}

numeric_cols <- sapply(data, is.numeric) 
for (col in names(data)[numeric_cols]) {
  hist(data[[col]], main = paste("Histogram of", col), xlab = col, col = "lightgreen")
}

numeric_cols <- sapply(data, is.numeric)  
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

pairs(categorical_encoded[, sapply(categorical_encoded, is.numeric)], 
      col = colors, 
      pch = 19, 
      main = "Scatterplot Matrix")

numeric_columns <- names(categorical_encoded)[sapply(categorical_encoded, is.numeric)]
for (col in numeric_columns) {
  if (col == "Price") next
  plot <- ggplot(categorical_encoded, aes_string(x = "Price_labels", y = col)) +
    geom_violin(fill = "lightgreen", color = "black") +  
    geom_boxplot(width = 0.1, outlier.color = "red") +  
    labs(title = paste("Violin Plot of", col, "by Price Labels"), 
         x = "Price Labels", 
         y = col) +
    theme_minimal()
  
  print(plot)
}

numeric_cols <- names(categorical_encoded)[sapply(categorical_encoded, is.numeric)]

for (i in 1:(length(numeric_cols) - 1)) {
  for (j in (i + 1):length(numeric_cols)) {
    combo <- c(numeric_cols[i], numeric_cols[j])
    
    plot_data <- data.frame(
      x = categorical_encoded[[combo[1]]][1:30],
      y = categorical_encoded[[combo[2]]][1:30]
    )
    
    plot <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_line(size = 1) +  
      geom_point(size = 3) +  
      labs(x = combo[1], y = combo[2], title = paste("Line Graph for", paste(combo, collapse = " vs. "))) +
      theme_minimal()
    
    print(plot)
  }
}
