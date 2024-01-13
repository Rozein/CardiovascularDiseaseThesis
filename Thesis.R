### Student_Name - Rozeen Nakarmmi
### Student_ID - 16034860

#installing packages
install.packages(c("dplyr", "ggplot2", "plotly", "RColorBrewer", "corrplot", "gridExtra", "randomForest", "tree", "xgboost", "ROCR", "naivebayes"))

#importing packages
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(corrplot)
library(gridExtra)
library(randomForest)
library(tree)
library(xgboost)
library(ROCR)
library(naivebayes)





# Setting the working Directory

setwd("C:/Islington Thesis/Thesis/archive (2)")

getwd() # To check the working directory

# Read the dataset
cardio_data <- read.csv("cardio_train.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)

# Explore the dataset
cat("Exploratory Data Analysis:\n")
head(cardio_data)  # Check dataset
View(cardio_data)  # View the dataset
sapply(cardio_data, class)  # View the type of columns
dim(cardio_data)  # Check dimension of dataset
sum(is.na(cardio_data))  # Check for null values in the dataset

# Print summary statistics before removing outliers
cat("Summary statistics before removing outliers:\n")
print(summary(cardio_data))

# Check data distribution with a boxplot before removing outliers
boxplot(cardio_data$height, cardio_data$weight, names = c("Height", "Weight"), main = "Boxplot before removing outliers")

# Function to remove outliers based on column and quantile limits
remove_outliers <- function(data, column, lower_quantile, upper_quantile) {
  lower_limit <- quantile(data[[column]], lower_quantile)
  upper_limit <- quantile(data[[column]], upper_quantile)
  outliers_mask <- data[[column]] < lower_limit | data[[column]] > upper_limit
  return(data[!outliers_mask, , drop = FALSE])
}

# Remove outliers for 'height' and 'weight'
cat("Removing outliers for 'height' and 'weight':\n")
cardio_data <- remove_outliers(cardio_data, "height", 0.025, 0.975)
cardio_data <- remove_outliers(cardio_data, "weight", 0.025, 0.975)

# Print summary statistics after removing outliers
cat("Summary statistics after removing height and weight outliers:\n")
print(summary(cardio_data))

# Check data distribution with a boxplot after removing outliers for height and weight
boxplot(cardio_data$height, cardio_data$weight, names = c("Height", "Weight"), main = "Boxplot after removing height and weight outliers")

# Count records where diastolic pressure is higher than systolic
inaccurate_blood_pressure <- sum(cardio_data$ap_lo > cardio_data$ap_hi)
cat("Diastolic pressure is higher than systolic one in", inaccurate_blood_pressure, "cases\n")
# Remove outliers for 'ap_hi' and 'ap_lo'
cat("Removing outliers for 'ap_hi' and 'ap_lo':\n")
cardio_data <- cardio_data %>%
  filter(!(ap_hi > 220 | ap_lo > 180 | ap_hi < 40 | ap_lo < 40))

# Print summary statistics after removing outliers for ap_hi and ap_lo
cat("Summary statistics after removing ap_hi and ap_lo outliers:\n")
print(summary(cardio_data))

inaccurate_blood_pressure <- sum(cardio_data$ap_lo > cardio_data$ap_hi)
cat("Diastolic pressure is higher than systolic one in", inaccurate_blood_pressure, "cases\n")

# Check dimension of the cleaned dataset
dim(cardio_data)

##Transforming Data

# Transforming the column 'age' (measured in days) to 'years'
cardio_data$years <- as.integer(round(cardio_data$age / 365))
cardio_data <- cardio_data[, !(names(cardio_data) %in% c("age", "id"))]  # Drop 'age' and 'id' columns

head(cardio_data)

# BMI formulation
cardio_data$bmi <- round(cardio_data$weight / (cardio_data$height / 100)^2, 2)

# If you want to see the resulting data frame
cat("Dimensions after adding 'bmi' column:", dim(cardio_data), "\n")
head(cardio_data)

print(cardio_data[cardio_data$bmi > 60 | cardio_data$bmi < 15, ])

# Filter out rows where BMI is greater than 60 or less than 15
cardio_data <- cardio_data[!(cardio_data$bmi > 60 | cardio_data$bmi < 15), ]
head(cardio_data)

# Transforming the 'bmi' column into BMI classes (1 to 6)
# Initialize an empty vector to store the ratings
rating <- numeric(length = nrow(cardio_data))

# Loop through the 'bmi' column and assign ratings
for (i in seq_len(nrow(cardio_data))) {
  if (cardio_data$bmi[i] < 18.5) {
    rating[i] <- 1  # Underweight
  } else if (cardio_data$bmi[i] >= 18.5 && cardio_data$bmi[i] < 24.9) {
    rating[i] <- 2  # Normal weight
  } else if (cardio_data$bmi[i] >= 24.9 && cardio_data$bmi[i] < 29.9) {
    rating[i] <- 3  # Overweight
  } else if (cardio_data$bmi[i] >= 29.9 && cardio_data$bmi[i] < 34.9) {
    rating[i] <- 4  # Class Obesity 1
  } else if (cardio_data$bmi[i] >= 34.9 && cardio_data$bmi[i] < 39.9) {
    rating[i] <- 5  # Class Obesity 2
  } else if (cardio_data$bmi[i] >= 39.9 && cardio_data$bmi[i] < 49.9) {
    rating[i] <- 6  # Class Obesity 3
  } else {
    rating[i] <- NA  # Error or Not Rated
  }
}

# Add the 'rating' column to the data frame
cardio_data$bmi_category <- rating

# If you want to see the resulting data frame
head(cardio_data)

# Calculate and display normalized counts of BMI classes
# Create a table of counts for each BMI class
class_counts <- table(cardio_data$bmi_category)

# Calculate the normalized counts
class_percentages <- prop.table(class_counts)

# Display the normalized counts
print(class_percentages)

# Function to categorize blood pressure
BPCategorize <- function(x, y) {
  if (x <= 120 & y <= 80) {
    return('normal')
  } else if (x <= 129 & y <= 80) {
    return('elevated')
  } else if (x <= 139 | y <= 89) {
    return('high 1')
  } else if (x <= 180 | y <= 120) {
    return('high 2')
  } else if (x > 180 | y > 120) {
    return('high 3')
  } else {
    return(NULL)
  }
}

# Apply the categorization function to create a new column 'bp_cat'
cardio_data <- cardio_data %>%
  mutate(bp_category = mapply(BPCategorize, ap_hi, ap_lo))

# Display the counts of each category
table(cardio_data$bp_category)
cardio_data$bmi_category <- factor(cardio_data$bmi_category)

#Data Visulization
# Bar plot for Categorical variable for bmi_category
gg_plot <- ggplot(cardio_data, aes(x = bmi_category, fill = factor(cardio))) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot for Categorical Variable 'bmi_category'", x = "bmi_category", y = "Count") +
  theme_minimal()

print(gg_plot)

# Blood Pressure Analysis
bp_boxplot <- ggplot(cardio_data, aes(x = factor(cardio), y = ap_hi, fill = factor(cardio))) +
  geom_boxplot() +
  labs(title = "Boxplot of Systolic Blood Pressure by Cardiovascular Risk", x = "Cardio", y = "Systolic Blood Pressure") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()
bp_boxplot

gender_boxplot <- ggplot(cardio_data, aes(x = factor(gender), y = bmi, fill = factor(gender))) +
  geom_boxplot() +
  labs(title = "Boxplot of BMI by Gender", x = "Gender", y = "BMI") +
  scale_fill_manual(values = c("1" = "pink", "2" = "lightblue")) +
  theme_minimal()
gender_boxplot

# Assuming gender is coded as 1 for female and 2 for male
gender_bmi_class_boxplot <- ggplot(cardio_data, aes(x = factor(bmi_category), y = bmi, fill = factor(gender))) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "white") +
  labs(title = "Boxplot of BMI by BMI Class and Gender", x = "BMI Class", y = "BMI", fill = "Gender") +
  scale_fill_manual(values = c("1" = "pink", "2" = "lightblue")) +
  theme_minimal()

gender_bmi_class_boxplot

# Using ggplot2 for visualization
ggplot(cardio_data[cardio_data$cardio == 1, ], aes(x = years)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True",
       x = "Age",
       y = "Density")

# Create a density plot for males
ggplot(subset(cardio_data, cardio == 1 & gender == 2), aes(x = years)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True (Males)",
       x = "Age",
       y = "Density")

# Create a density plot for females
ggplot(subset(cardio_data, cardio == 1 & gender == 1), aes(x = years)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True (Females)",
       x = "Age",
       y = "Density")

head(cardio_data)

ggplot(cardio_data[cardio_data$cardio == 1, ], aes(x = gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Cardio Cases by Gender",
       x = "Gender",
       y = "Count") +
  facet_wrap(~gender, scales = "free_y")

# Plot 1: Cardio percentage
cardio_counts <- table(cardio_data$cardio)
labels_cardio <- c("Cardio", "No cardio")
percentages_cardio <- cardio_counts / sum(cardio_counts) * 100

plot_ly(labels = labels_cardio, values = cardio_counts, type = "pie",
        marker = list(colors = c("skyblue", "lightcoral")),
        textinfo = "label+percent", hole = 0.3) %>%
  layout(title = "Cardio percentage", showlegend = TRUE, legend = list(x = 1, y = 1))

# Plot 2: Cardiovascular patients gender percentage
gender_counts <- table(ifelse(cardio_data$cardio == 1, ifelse(cardio_data$gender == 1, "Female", "Male"), NA))
labels_gender <- c("Female", "Male")
percentages_gender <- gender_counts / sum(gender_counts, na.rm = TRUE) * 100

plot_ly(labels = labels_gender, values = gender_counts, type = "pie",
        marker = list(colors = c("pink", "lightblue")),
        textinfo = "label+percent", hole = 0.3) %>%
  layout(title = "Cardiovascular patients gender percentage", showlegend = TRUE, legend = list(x = 1, y = 1))

head(cardio_data)

# Smoke use for gender
p1 <- ggplot(cardio_data, aes(x = smoke, fill = factor(gender))) +
  geom_bar(position = "dodge") +
  labs(title = "Smoke use for gender")

# Alcohol use for gender
p2 <- ggplot(cardio_data, aes(x = alco, fill = factor(gender))) +
  geom_bar(position = "dodge") +
  labs(title = "Alcohol use for gender")


grid.arrange(p1, p2, ncol = 2)

# Create a function to generate count plots
generate_gluc_count_plot <- function(data, title) {
  ggplot(data, aes(x = gluc, fill = factor(cardio))) +
    geom_bar(position = "dodge") +
    labs(title = title) +
    theme_minimal()
}
# Filter data for males and females
male_data <- cardio_data[cardio_data$gender == 2, ]
female_data <- cardio_data[cardio_data$gender == 1, ]
# Create plots for all data, male data, and female data
plot_gluc_all <- generate_gluc_count_plot(cardio_data, "All Data")
plot_gluc_male <- generate_gluc_count_plot(male_data, "Male Data")
plot_gluc_female <- generate_gluc_count_plot(female_data, "Female Data")

head(cardio_data)
# Arrange the plots in a single row
library(gridExtra)
grid.arrange(plot_gluc_all, plot_gluc_male, plot_gluc_female, ncol = 3)

# Create a function to generate count plots
generate_chol_count_plot <- function(data, title) {
  ggplot(data, aes(x = cholesterol, fill = factor(cardio))) +
    geom_bar(position = "dodge") +
    labs(title = title) +
    theme_minimal()
}

plot_chol_all <- generate_chol_count_plot(cardio_data, "All Data")
plot_chol_male <- generate_chol_count_plot(male_data, "Male Data")
plot_chol_female <- generate_chol_count_plot(female_data, "Female Data")
grid.arrange(plot_chol_all, plot_chol_male, plot_chol_female, ncol = 3)

generate_smoke_count_plot <- function(data, title) {
  ggplot(data, aes(x = smoke, fill = factor(cardio))) +
    geom_bar(position = "dodge") +
    labs(title = title) +
    theme_minimal()
}

plot_smoke_all <- generate_smoke_count_plot(cardio_data, "All Data")
plot_smoke_male <- generate_smoke_count_plot(male_data, "Male Data")
plot_smoke_female <- generate_smoke_count_plot(female_data, "Female Data")
grid.arrange(plot_smoke_all, plot_smoke_male, plot_smoke_female, ncol = 3)

generate_active_count_plot <- function(data, title) {
  ggplot(data, aes(x = factor(active), fill = factor(cardio))) +
    geom_bar(position = "dodge") +
    labs(title = title) +
    theme_minimal() +
    scale_x_discrete(labels = c("Inactive", "Active"))
}

plot_active_all <- generate_active_count_plot(cardio_data, "All Data")
plot_active_male <- generate_active_count_plot(male_data, "Male Data")
plot_active_female <- generate_active_count_plot(female_data, "Female Data")
grid.arrange(plot_active_all, plot_active_male, plot_active_female, ncol = 3)

#Probability and Statistics
# Subset data for individuals aged 50 and above
cardio_data_age_50 <- subset(cardio_data, years >= 50)

# Subset data for individuals aged 50 and above with cardio=1
cardio_data_age_50_cardio <- subset(cardio_data_age_50, cardio == 1)

# Calculate the percentage
percentage_age_50_cardio_data <- round((nrow(cardio_data_age_50_cardio) / nrow(cardio_data_age_50)) * 100, 2)

# Print the result
cat("Percentage of individuals aged 50 and above with cardio=1:", percentage_age_50_cardio_data, "%\n")

# Subset data for individuals with BMI >= 37
df_bmi37 <- subset(cardio_data, bmi >= 37)

# Subset data for individuals with BMI >= 37 and cardio = 1
df_bmi37_cardio <- subset(df_bmi37, cardio == 1)

# Calculate the percentage
percentage_bmi37_cardio <- round((nrow(df_bmi37_cardio) / nrow(df_bmi37)) * 100, 2)

# Print the result
cat("Percentage of individuals with BMI >= 37 and cardio=1:", percentage_bmi37_cardio, "%\n")

# Subset data for 'bp_cat' equal to 'high 3'
df_high3 <- filter(cardio_data, bp_category == 'high 2')

# Subset data for 'bp_cat' equal to 'high 3' and 'cardio' equal to 1
df_high_cardio <- filter(df_high3, cardio == 1)

# Calculate the percentage
percentage_high_cardio <- round((nrow(df_high_cardio) / nrow(df_high3)) * 100, 2)

# Print the result
cat("Percentage of individuals with 'bp_cat' equal to 'high 3' and 'cardio' equal to 1:", percentage_high_cardio, "%\n")

# Subset data for 'alco' equal to 1 or 'smoke' equal to 1
df_cohol_smoke <- filter(cardio_data, alco == 1 | smoke == 1)

# Calculate the percentage
percentage_cohol_smoke <- round((nrow(df_cohol_smoke) / nrow(cardio_data)) * 100, 2)

# Print the result
cat("Percentage of individuals with 'alco' equal to 1 or 'smoke' equal to 1:", percentage_cohol_smoke, "%\n")

df_cohol_smoke_cardio <- filter(df_cohol_smoke, cardio == 1)

# Calculate the percentage
percentage_cohol_smoke_cardio <- round((nrow(df_cohol_smoke_cardio) / nrow(df_cohol_smoke)) * 100, 2)

# Print the result
cat("Percentage of individuals with 'alco' equal to 1 or 'smoke' equal to 1 and 'cardio' equal to 1:", percentage_cohol_smoke_cardio, "%\n")

# Subset data for 'active' equal to 0
df_not_active <- filter(cardio_data, active == 0)

# Subset data for individuals with 'cardio' equal to 1
df_not_active_cardio <- filter(df_not_active, cardio == 1)

# Calculate the percentage
percentage_not_active_cardio <- round((nrow(df_not_active_cardio) / nrow(df_not_active)) * 100, 2)

# Print the result
cat("Percentage of individuals with 'active' equal to 0 and 'cardio' equal to 1:", percentage_not_active_cardio, "%\n")

head(cardio_data)

##Machine learning
##Data preparation
sapply(cardio_data, class)## Check data type of cardio_data

char_cols <- which(sapply(cardio_data, is.character)) ##Finding all character columns
char_cols

cardio_data_filtered <- cardio_data %>% select(-char_cols) #Removing Character Columns
head(cardio_data_filtered)
cardio_data_filtered$bmi_category <- as.integer(cardio_data_filtered$bmi_category)
sapply(cardio_data_filtered, class)
cardio_data_filtered_scaled <- cardio_data_filtered %>%
  scale() %>% as.data.frame()

head(cardio_data_filtered_scaled)

set.seed(123)  # Set seed for reproducibility
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))

# Split the data into training and testing sets
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]
# Perform linear regression on the training data
model <- lm(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years + bmi, data = train_data)

# Display summary of the linear regression model
summary(model)

# Make predictions on the testing data
predictions <- predict(model, newdata = test_data)


# Evaluate the model performance (you can use different metrics)
# For example, mean squared error (MSE)
mse <- mean((test_data$cardio - predictions)^2)
print(paste("Mean Squared Error:", mse))


train_data$cardio <- as.factor(train_data$cardio)
test_data$cardio <- as.factor(test_data$cardio)

# Perform Random Forest classification on the training data
model_rf <- randomForest(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years + bmi, data = train_data)

# Make predictions on the testing data
predictions_rf <- predict(model_rf, newdata = test_data)


# Evaluate the model performance
conf_matrix <- table(predictions_rf, test_data$cardio)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy, sensitivity, specificity, etc.
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

##Tree
# Assuming your dataframe is named cardio_data_filtered_scaled
set.seed(123)  # Set seed for reproducibility

# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
cardio_data_filtered_scaled$cardio <- as.factor(cardio_data_filtered_scaled$cardio)
# Split the data into training and testing sets
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]

# Build a decision tree model
model_tree <- tree(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years + bmi, data = train_data)

# Plot the decision tree (optional)
plot(model_tree)
text(model_tree, pretty = 0)

# Make predictions on the testing data
predictions_tree <- predict(model_tree, newdata = test_data, type = "class")

# Evaluate the model performance
conf_matrix_tree <- table(predictions_tree, test_data$cardio)
print("Confusion Matrix:")
print(conf_matrix_tree)

# Calculate accuracy, sensitivity, specificity, etc.
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
sensitivity_tree <- conf_matrix_tree[2, 2] / sum(conf_matrix_tree[2, ])
specificity_tree <- conf_matrix_tree[1, 1] / sum(conf_matrix_tree[1, ])

print(paste("Accuracy:", accuracy_tree))
print(paste("Sensitivity:", sensitivity_tree))
print(paste("Specificity:", specificity_tree))

#XGBOOST Regression
# Assuming your dataframe is named cardio_data_filtered_scaled
set.seed(123)  # Set seed for reproducibility

# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
head(cardio_data)
cardio_data_filtered_scaled$cardio <- cardio_data$cardio
head(cardio_data_filtered_scaled)
# Split the data into training and testing sets
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]

# Build an XGBoost model
model_xgb <- xgboost(data = as.matrix(train_data[, !colnames(train_data) %in% "cardio"]),
                     label = train_data$cardio,
                     objective = "binary:logistic",
                     nrounds = 100)

# Make predictions on the testing data
predictions_xgb <- predict(model_xgb, as.matrix(test_data[, !colnames(test_data) %in% "cardio"]))

# Convert predicted probabilities to class labels
predictions_xgb_class <- ifelse(predictions_xgb > 0.5, 1, 0)

# Evaluate the model performance
prediction_obj <- prediction(predictions_xgb, test_data$cardio)
performance_obj <- performance(prediction_obj, "tpr", "fpr")

# Plot ROC curve
plot(performance_obj, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
# Convert predicted probabilities to class labels
predictions_xgb_class <- ifelse(predictions_xgb > 0.5, 1, 0)

# Compare predicted labels to actual labels
accuracy <- sum(predictions_xgb_class == test_data$cardio) / length(test_data$cardio)

print(paste("Accuracy:", accuracy))

##naive

set.seed(123)  # Set seed for reproducibility

# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
cardio_data_filtered_scaled$cardio <- as.factor(cardio_data_filtered_scaled$cardio)

# Split the data into training and testing sets
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]

# Define the features used for training
features <- c("gender", "height", "weight", "ap_hi", "ap_lo", "cholesterol", "gluc", "smoke", "alco", "active", "years", "bmi")

# Build a Naive Bayes model
model_nb <- naive_bayes(cardio ~ ., data = train_data[, c(features, "cardio")])

# Select only the relevant features for prediction
newdata_for_prediction <- test_data[, features]

# Make predictions on the testing data
predictions_nb <- predict(model_nb, newdata = newdata_for_prediction)

# Extract predicted classes
predicted_classes <- as.numeric(predictions_nb)

# Evaluate the model performance
conf_matrix_nb <- table(predicted_classes, test_data$cardio)
print("Confusion Matrix:")
print(conf_matrix_nb)

# Calculate accuracy, sensitivity, specificity, etc.
accuracy_nb <- sum(diag(conf_matrix_nb)) / sum(conf_matrix_nb)
sensitivity_nb <- conf_matrix_nb[2, 2] / sum(conf_matrix_nb[2, ])
specificity_nb <- conf_matrix_nb[1, 1] / sum(conf_matrix_nb[1, ])

print(paste("Accuracy:", accuracy_nb))
print(paste("Sensitivity:", sensitivity_nb))
print(paste("Specificity:", specificity_nb))

