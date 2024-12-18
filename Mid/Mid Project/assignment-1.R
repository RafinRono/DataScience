install.packages("dplyr")
install.packages('openxlsx')
install.packages("stringdist")

library(dplyr)
library(openxlsx)
library(stringdist)

data <- read.xlsx("Midterm_Dataset_Section(C).xlsx")

str(data)
summary(data)

nrow(data)
nrow(distinct(data))

distinct_data <- distinct(data)


annotated <- distinct_data
annotated$loan_status <- factor(annotated$loan_status, 
                                levels = c(0, 1), 
                                labels = c("rejected", "accepted"))


categorical_cols <- names(annotated)[sapply(annotated, function(x) is.factor(x) | is.character(x))]
categorical_cols

plotCategoricalCols <- function(data = annotated, col)
{
  counts <- table(data[[col]]) 
  bar_positions <- barplot(counts, 
                           main = paste("Distribution of ", col),
                           col = "orange", 
                           xlab = "", 
                           ylab = "Frequency", 
                           cex.lab = 0.8,
                           cex.names = 0.7,
                           las = 2)
  
  text(bar_positions, counts, labels = counts, pos = 1, cex = 1)
}

plotCategoricalCols(annotated, "person_gender")
plotCategoricalCols(annotated, "person_education")
plotCategoricalCols(annotated, "person_home_ownership")
plotCategoricalCols(annotated, "loan_intent")
plotCategoricalCols(annotated, "previous_loan_defaults_on_file")
plotCategoricalCols(annotated, "loan_status")


fixed_invalid_categorical <- annotated

valid_values <- c("MORTGAGE", "OWN", "OTHER", "RENT")

fix_values <- function(column, valid_values) {
  sapply(column, function(value) {
    closest <- valid_values[which.min(stringdist::stringdist(value, valid_values))]
    return(closest)
  })
}

fixed_invalid_categorical <- fixed_invalid_categorical %>%
  mutate(person_home_ownership = fix_values(person_home_ownership, valid_values))

plotCategoricalCols(fixed_invalid_categorical, "person_home_ownership")


categorical_cols

lowered <- fixed_invalid_categorical
for (col in categorical_cols) {
  lowered[[col]] <- tolower(lowered[[col]])
}


colSums(is.na(lowered[categorical_cols]))
for (col_name in categorical_cols)
{
  cat(col_name, " -> ", which(is.na(lowered[col_name])), "\n")
}

barplot(colSums(is.na(lowered[categorical_cols])), las = 2, col = "blue", 
        main = "Missing Values per Categorical Column", 
        xlab = "", ylab = "Count of missing Values", 
        cex.lab = 0.9,      
        cex.names = 0.7)    


discraded_null <- lowered
discraded_null <- na.omit(discraded_null)


bottom_up <- lowered
categorical_cols <- names(bottom_up)[sapply(bottom_up, function(x) is.factor(x) | is.character(x))]

for (col in categorical_cols) {
  for (i in seq_len(nrow(bottom_up) - 1)) {
    if (is.na(bottom_up[[col]][i])) {
      bottom_up[[col]][i] <- bottom_up[[col]][i + 1]
    }
  }
}


top_down <- lowered
categorical_cols <- names(top_down)[sapply(top_down, function(x) is.factor(x) | is.character(x))]
for (col in categorical_cols) {
  for (i in seq_len(nrow(top_down))[-1]) {
    if (is.na(top_down[[col]][i])) {
      top_down[[col]][i] <- top_down[[col]][i - 1]
    }
  }
}


most_frequent_data <- lowered
categorical_cols <- names(most_frequent_data)[sapply(most_frequent_data, function(x) is.factor(x) | is.character(x))]

for (col in categorical_cols) {
  most_frequent <- names(sort(table(most_frequent_data[[col]]), decreasing = TRUE))[1]
  
  most_frequent_data[[col]][which(is.na(most_frequent_data[[col]]))] <- most_frequent
}



label_encoded <- most_frequent_data 

gender <- unique(label_encoded$person_gender)
label_encoded$person_gender <- factor(label_encoded$person_gender, 
                                      levels = gender, 
                                      labels = 0:(length(gender) - 1))

education <- unique(label_encoded$person_education)
label_encoded$person_education <- factor(label_encoded$person_education, 
                                         levels = education, 
                                         labels = 0:(length(education) - 1))

home_ownership <- unique(label_encoded$person_home_ownership)
label_encoded$person_home_ownership <- factor(label_encoded$person_home_ownership, 
                                              levels = home_ownership, 
                                              labels = 0:(length(home_ownership) - 1))

intent <- unique(label_encoded$loan_intent)
label_encoded$loan_intent <- factor(label_encoded$loan_intent, 
                                    levels = intent, 
                                    labels = 0:(length(intent) - 1))

loan_defaults_on_file <- unique(label_encoded$previous_loan_defaults_on_file)
label_encoded$previous_loan_defaults_on_file <- factor(label_encoded$previous_loan_defaults_on_file, 
                                                       levels = loan_defaults_on_file, 
                                                       labels = 0:(length(loan_defaults_on_file) - 1))



str(label_encoded)
numeric_cols <- names(label_encoded)[sapply(label_encoded, is.numeric)]
summary(label_encoded[numeric_cols])



plotFreq <- function(col_name)
{
  barplot(table(data[[col_name]]),
          main = paste("Mean value for ", col_name, ": ", mean(data[[col_name]], na.rm = TRUE)),
          col = "skyblue",
          xlab = col_name,
          ylab = "Frequency",
          las = 2)
}

plotFreq("person_age")
plotFreq("person_income")
plotFreq("person_emp_exp")
plotFreq("loan_amnt")
plotFreq("loan_int_rate")
plotFreq("loan_percent_income")
plotFreq("cb_person_cred_hist_length")
plotFreq("credit_score")


colSums(is.na(label_encoded))
for (col_name in numeric_cols)
{
  cat(col_name, " -> ", which(is.na(label_encoded[col_name])), "\n")
}

barplot(colSums(is.na(label_encoded[numeric_cols])), las = 2, col = "blue", 
        main = "Missing Values per Numeric Column", 
        xlab = "", ylab = "Count of missing Values", 
        cex.lab = 0.9,      
        cex.names = 0.9) 


discraded_null_numeric <- label_encoded
discraded_null_numeric <- na.omit(discraded_null_numeric)



top_down_numeric_null <- label_encoded

for (col in numeric_cols) {
  for (i in seq_len(nrow(top_down_numeric_null))[-1]) {  
    if (is.na(top_down_numeric_null[[col]][i])) {
      top_down_numeric_null[[col]][i] <- top_down_numeric_null[[col]][i - 1]
    }
  }
}


bottom_up_numeric_null <- label_encoded

for (col in numeric_cols) {
  for (i in seq_len(nrow(bottom_up_numeric_null) - 1)) {
    if (is.na(bottom_up_numeric_null[[col]][i])) {
      bottom_up_numeric_null[[col]][i] <- bottom_up_numeric_null[[col]][i + 1]
    }
  }
}


mode_replaced_numeric_null <- label_encoded

for (col in numeric_cols) {
  most_frequent <- names(sort(table(mode_replaced_numeric_null[[col]]), decreasing = TRUE))[1]
  mode_replaced_numeric_null[[col]][which(is.na(mode_replaced_numeric_null[[col]]))] <- most_frequent
}


mean_replaced_numeric_null <- label_encoded

for (col in numeric_cols) {
  if (any(is.na(mean_replaced_numeric_null[[col]]))) {
    mean_value <- round(mean(mean_replaced_numeric_null[[col]], na.rm = TRUE))
    mean_replaced_numeric_null[[col]][which(is.na(mean_replaced_numeric_null[[col]]))] <- mean_value
  }
}


median_replaced_numeric_null <- label_encoded

for (col in numeric_cols) {
  if (any(is.na(median_replaced_numeric_null[[col]]))) {
    median_value <- median(median_replaced_numeric_null[[col]], na.rm = TRUE)
    median_replaced_numeric_null[[col]][which(is.na(median_replaced_numeric_null[[col]]))] <- median_value
  }
}


median_replaced_numeric_null %>% summarise_if(is.numeric, sd)


z_score_outlier_handeled <- median_replaced_numeric_null
for (col in numeric_cols) {
  z_scores <- scale(z_score_outlier_handeled[[col]])
  
  z_score_outlier_handeled <- z_score_outlier_handeled[abs(z_scores) <= 3, ]
}



iqr_outlier_handled <- median_replaced_numeric_null

for (col in numeric_cols) {
  Q1 <- quantile(iqr_outlier_handled[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(iqr_outlier_handled[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  iqr_outlier_handled <- iqr_outlier_handled[iqr_outlier_handled[[col]] >= lower_bound & iqr_outlier_handled[[col]] <= upper_bound, ]
}



sapply(z_score_outlier_handeled[numeric_cols], function(x) if(is.numeric(x)) sd(x, na.rm = TRUE))

chi_squared <- z_score_outlier_handeled

person_income_bins <- cut(chi_squared$person_income, breaks = 4)
levels(person_income_bins)  
levels(person_income_bins) <- c("Low", "Lower Middle", "Upper Middle", "High")
chi_squared$person_income <- person_income_bins

amount <- cut(chi_squared$loan_amnt, breaks = 3)
levels(amount) 
levels(amount) <- c("Small", "Medium", "Large")
chi_squared$loan_amnt <- amount

str(chi_squared)


normalized_numeric  <- chi_squared

col_min <- min(normalized_numeric[["credit_score"]])
col_max <- max(normalized_numeric[["credit_score"]])
normalized_numeric[["credit_score"]] <- (normalized_numeric[["credit_score"]] - col_min) / (col_max - col_min)

normalized_numeric[["loan_int_rate"]] <- (normalized_numeric[["loan_int_rate"]] / 100)


normalized_numeric_filtered <- normalized_numeric %>% filter(person_age < 80)


balanced_data <- normalized_numeric_filtered
table(balanced_data$loan_status)
plotCategoricalCols(balanced_data, "loan_status")

minority_class <- filter(balanced_data, loan_status == "rejected")
majority_class <- filter(balanced_data, loan_status == "accepted")

num_to_add <- nrow(majority_class) - nrow(minority_class)
num_to_add <- num_to_add + 20

upsampled_minority <- slice_sample(minority_class, n = num_to_add, replace = FALSE)

balanced_data <- bind_rows(majority_class, minority_class, upsampled_minority)

table(balanced_data$loan_status)

plotCategoricalCols(balanced_data, "loan_status")



minority_class <- filter(balanced_data, loan_status == "accepted")
majority_class <- filter(balanced_data, loan_status == "rejected")

downsampled_majority_class <- majority_class %>% sample_n(nrow(minority_class))

balanced_data <- bind_rows(downsampled_majority_class, minority_class)

table(balanced_data$loan_status)
plotCategoricalCols(balanced_data, "loan_status")


str(balanced_data)
summary(balanced_data)


write.xlsx(balanced_data, "data_preprocessed.xlsx")
