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
library(tidyverse)
library(caret)


# Setting the working Directory

setwd("C:/Islington Thesis/Thesis")

getwd() # To check the working directory

# Read the dataset
cardio_data <- read.csv("cardio_train.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)

# Explore the dataset
head(cardio_data)  # Check dataset
View(cardio_data)  # View the dataset
sapply(cardio_data, class)  # View the type of columns
dim(cardio_data)  # Check dimension of dataset
sum(is.na(cardio_data))  # Check for null values in the dataset

summary(cardio_data)
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
cardio_data <- remove_outliers(cardio_data, "height", 0.025, 0.975)
cardio_data <- remove_outliers(cardio_data, "weight", 0.025, 0.975)


# Check data distribution with a boxplot after removing outliers for height and weight
boxplot(cardio_data$height, cardio_data$weight, names = c("Height", "Weight"), main = "Boxplot after removing height and weight outliers")

# Count records where diastolic pressure is higher than systolic
inaccurate_blood_pressure <- sum(cardio_data$ap_lo > cardio_data$ap_hi)
cat("Diastolic pressure is higher than systolic one in", inaccurate_blood_pressure, "cases\n")
# Remove outliers for 'ap_hi' and 'ap_lo'
cardio_data <- cardio_data %>%
  filter(!(ap_hi > 220 | ap_lo > 180 | ap_hi < 40 | ap_lo < 40 | ap_lo > ap_hi))

inaccurate_blood_pressure <- sum(cardio_data$ap_lo > cardio_data$ap_hi)
cat("Diastolic pressure is higher than systolic one in", inaccurate_blood_pressure, "cases\n")

# Print summary statistics after removing outliers for ap_hi and ap_lo
print(summary(cardio_data))



# Check dimension of the cleaned dataset
dim(cardio_data)


# Transforming the column 'age' (measured in days) to 'years'
head(cardio_data)
cardio_data$years <- as.integer(round(cardio_data$age / 365))
cardio_data <- cardio_data[, !(names(cardio_data) %in% c("age", "id"))]  # Drop 'age' and 'id' columns

head(cardio_data)

# BMI formulation
cardio_data$bmi <- round(cardio_data$weight / (cardio_data$height / 100)^2, 2)

cat("Dimensions after adding 'bmi' column:", dim(cardio_data), "\n")

print(cardio_data[cardio_data$bmi > 60 | cardio_data$bmi < 15, ])

# Filter out rows where BMI is greater than 60 or less than 15
cardio_data <- cardio_data[!(cardio_data$bmi > 60 | cardio_data$bmi < 15), ]

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

head(cardio_data)

# Calculate and display normalized counts of BMI classes
# Create a table of counts for each BMI class
class_counts <- table(cardio_data$bmi_category)
class_counts
# Calculate the normalized counts
class_percentages <- prop.table(class_counts) * 100

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

head(cardio_data)

# Display the counts of each category
table(cardio_data$bp_category)
cardio_data$bmi_category <- factor(cardio_data$bmi_category)

#Check cardio percentage in dataset
cardio_counts <- table(cardio_data$cardio)
labels_cardio <- c("Cardio", "No cardio")
percentages_cardio <- cardio_counts / sum(cardio_counts) * 100

plot_ly(labels = labels_cardio, values = cardio_counts, type = "pie",
        marker = list(colors = c("skyblue", "lightcoral")),
        textinfo = "label+percent", hole = 0.3) %>%
  layout(title = "Cardio percentage", showlegend = TRUE, legend = list(x = 1, y = 1))

# Cardiovascular patients gender percentage
gender_counts <- table(ifelse(cardio_data$cardio == 1, ifelse(cardio_data$gender == 1, "Female", "Male"), NA))
labels_gender <- c("Female", "Male")
percentages_gender <- gender_counts / sum(gender_counts, na.rm = TRUE) * 100

plot_ly(labels = labels_gender, values = gender_counts, type = "pie",
        marker = list(colors = c("pink", "lightblue")),
        textinfo = "label+percent", hole = 0.3) %>%
  layout(title = "Cardiovascular patients gender percentage", showlegend = TRUE, legend = list(x = 1, y = 1))


gender_counts <- table(cardio_data$gender)
print(gender_counts)
cardio_counts <- table(cardio_data$gender[cardio_data$cardio == 1])
# Display the counts
print(cardio_counts)

head(cardio_data)

# Cardio according to bmi category

gg_plot <- ggplot(cardio_data, aes(x = bmi_category, fill = factor(cardio))) +
  geom_bar(position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Bar Plot for Categorical Variable 'bmi_category'", x = "bmi_category", y = "Count") +
  theme_minimal()

gg_plot

gender_bmi_class_barplot <- ggplot(cardio_data[cardio_data$cardio == 1, ], aes(x = factor(bmi_category), fill = factor(gender))) +
  geom_bar(position = position_dodge(width = 0.8), color = "white") +
  geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(title = "Barplot of BMI by BMI Class and Gender where people have cardio", x = "BMI Class", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("1" = "pink", "2" = "lightblue"), name = "Gender", labels = c("Female", "Male")) +
  theme_minimal()

gender_bmi_class_barplot


# Blood Pressure Analysis
bp_boxplot <- ggplot(cardio_data, aes(x = factor(cardio), y = ap_hi, fill = factor(cardio))) +
  geom_boxplot() +
  labs(title = "Boxplot of Systolic Blood Pressure by Cardiovascular Risk", x = "Cardio", y = "Systolic Blood Pressure") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()
bp_boxplot


#Cardio occurred according to age.
ggplot(cardio_data[cardio_data$cardio == 1, ], aes(x = years)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True",
       x = "Age",
       y = "Density")

# Cardio occurred according to age for males
ggplot(subset(cardio_data, cardio == 1 & gender == 2), aes(x = years)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True (Males)",
       x = "Age",
       y = "Density")

# Cardio occurred according to age for females
ggplot(subset(cardio_data, cardio == 1 & gender == 1), aes(x = years)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(title = "Density Plot of Age for Cardio = True (Females)",
       x = "Age",
       y = "Density")

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


##Evaluating the relationship between glucose levels and cardiovascular disease.
df_gluc <- aggregate(cardio ~ gluc, data = cardio_data, mean)
df_gluc_male <- aggregate(cardio ~ gluc, data = cardio_data[cardio_data$gender == 2, ], mean)
df_gluc_female <- aggregate(cardio ~ gluc, data = cardio_data[cardio_data$gender == 1, ], mean)
df_gluc$gluc <- factor(df_gluc$gluc, levels = c(1, 2, 3),
                       labels = c('normal', 'above normal', 'well above normal'))
df_gluc_male$gluc <- factor(df_gluc_male$gluc, levels = c(1, 2, 3),
                            labels = c('normal', 'above normal', 'well above normal'))
df_gluc_female$gluc <- factor(df_gluc_female$gluc, levels = c(1, 2, 3),
                              labels = c('normal', 'above normal', 'well above normal'))
generate_gluc_count_plot <- function(data, title) {
  ggplot(data, aes(x = gluc, y = cardio, fill = gluc)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
    labs(x = title, y = "Mean Cardio Value") +
    theme_minimal()
}
plot_gluc_all <- generate_gluc_count_plot(df_gluc, "All Data")
plot_gluc_male <- generate_gluc_count_plot(df_gluc_male, "Male Data")
plot_gluc_female <- generate_gluc_count_plot(df_gluc_female, "Female Data")

# Arrange the plots in a single row
grid.arrange(plot_gluc_all, plot_gluc_male, plot_gluc_female, ncol = 3)

## Create a function to generate count plots for cholesterol
df_chol <- aggregate(cardio ~ cholesterol, data = cardio_data, mean)
df_chol_male <- aggregate(cardio ~ cholesterol, data = cardio_data[cardio_data$gender == 2, ], mean)
df_chol_female <- aggregate(cardio ~ cholesterol, data = cardio_data[cardio_data$gender == 1, ], mean)
df_chol$cholesterol <- factor(df_chol$cholesterol, levels = c(1, 2, 3),
                       labels = c('normal', 'above normal', 'well above normal'))
df_chol_male$cholesterol <- factor(df_chol_male$cholesterol, levels = c(1, 2, 3),
                            labels = c('normal', 'above normal', 'well above normal'))
df_chol_female$cholesterol <- factor(df_chol_female$cholesterol, levels = c(1, 2, 3),
                              labels = c('normal', 'above normal', 'well above normal'))
generate_chol_count_plot <- function(data, title) {
  ggplot(data, aes(x = cholesterol, y = cardio, fill = cholesterol)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
    labs(x = title, y = "Mean Cardio Value") +
    theme_minimal()
}
plot_chol_all <- generate_chol_count_plot(df_chol, "All Data")
plot_chol_male <- generate_chol_count_plot(df_chol_male, "Male Data")
plot_chol_female <- generate_chol_count_plot(df_chol_female, "Female Data")

# Arrange the plots in a single row
grid.arrange(plot_chol_all, plot_chol_male, plot_chol_female, ncol = 3)

###
## Create a function to generate count plots for smoke
df_smoke <- aggregate(cardio ~ smoke, data = cardio_data, mean)
df_smoke_male <- aggregate(cardio ~ smoke, data = cardio_data[cardio_data$gender == 2, ], mean)
df_smoke_female <- aggregate(cardio ~ smoke, data = cardio_data[cardio_data$gender == 1, ], mean)
df_smoke$smoke <- factor(df_smoke$smoke, levels = c(0, 1),
                         labels = c('No Smoke', 'Smoke'))
df_smoke_male$smoke <- factor(df_smoke_male$smoke, levels = c(0, 1),
                                    labels = c('No Smoke', 'Smoke'))
df_smoke_female$smoke <- factor(df_smoke_female$smoke, levels = c(0, 1),
                              labels = c('No Smoke', 'Smoke'))
plot_smoke_all <- ggplot(df_smoke, aes(x = smoke, y = cardio, fill = smoke)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Smoking Status", y = "Mean Cardio Value") +
  theme_minimal()
plot_smoke_male <-ggplot(df_smoke_male, aes(x = smoke, y = cardio, fill = smoke)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Smoking Status", y = "Mean Cardio Value") +
  theme_minimal()
plot_smoke_male
plot_smoke_female <-ggplot(df_smoke_female, aes(x = smoke, y = cardio, fill = smoke)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Smoking Status", y = "Mean Cardio Value") +
  theme_minimal()
plot_smoke_female
grid.arrange(plot_smoke_all, plot_smoke_male, plot_smoke_female, ncol = 3)

# Group by 'alco' and calculate the mean for all genders
df_alco_all <- aggregate(cardio ~ alco, data = cardio_data, mean)
df_alco_male <- aggregate(cardio ~ alco, data = cardio_data[cardio_data$gender == 2, ], mean)
df_alco_female <- aggregate(cardio ~ alco, data = cardio_data[cardio_data$gender == 1, ], mean)

# Define the order of x-axis labels
df_alco_all$alco <- factor(df_alco_all$alco, levels = c(0, 1),
                           labels = c('No Alcohol', 'Alcohol'))
df_alco_male$alco <- factor(df_alco_male$alco, levels = c(0, 1),
                            labels = c('No Alcohol', 'Alcohol'))
df_alco_female$alco <- factor(df_alco_female$alco, levels = c(0, 1),
                              labels = c('No Alcohol', 'Alcohol'))

# Plot for all genders
plot_alco_all <- ggplot(df_alco_all, aes(x = alco, y = cardio, fill = alco)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Alcohol Consumption", y = "Mean Cardio Value") +
  theme_minimal()

# Plot for male gender
plot_alco_male <- ggplot(df_alco_male, aes(x = alco, y = cardio, fill = alco)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Alcohol Consumption", y = "Mean Cardio Value") +
  theme_minimal()

# Plot for female gender
plot_alco_female <- ggplot(df_alco_female, aes(x = alco, y = cardio, fill = alco)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1), labels = seq(0, 120, 10)) +
  labs(x = "Alcohol Consumption", y = "Mean Cardio Value") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot_alco_all, plot_alco_male, plot_alco_female, ncol = 3)

## Create a function to generate count plots for active
male_data <- cardio_data[cardio_data$gender == 2, ]
female_data <- cardio_data[cardio_data$gender == 1, ]

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

head(cardio_data)
# Group by 'bp_category' and calculate the mean
df_bp <- aggregate(cardio ~ bp_category, data = cardio_data, mean)

# Define the order of x-axis labels
bp_order <- c('normal', 'elevated', 'high 1', 'high 2', 'high 3')
df_bp$bp_category <- factor(df_bp$bp_category, levels = bp_order)

# Plot the grouped bar plot
plot_bp <- ggplot(df_bp, aes(x = bp_category, y = cardio, fill = bp_category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Blood Pressure Category", y = "Mean Cardio Value") +
  theme_minimal()

# Show the plot
print(plot_bp)
##Probability and Statistics
# Subset data for individuals aged 50 and above
cardio_data_age_50_70 <- subset(cardio_data, years >= 50 & years <= 70)
# Subset data for individuals aged 50 and above with cardio=1
cardio_data_age_50_70_cardio <- subset(cardio_data_age_50_70, cardio == 1)
# Calculate the percentage
percentage_age_50_70_cardio_data <- round((nrow(cardio_data_age_50_70_cardio) / nrow(cardio_data_age_50_70)) * 100, 2)
# Print the result
cat("Percentage of individuals aged between 50-70 with cardiovascular:", percentage_age_50_70_cardio_data, "%\n")

# Subset data for individuals with BMI >= 37
df_bmi37 <- subset(cardio_data, bmi >= 37)
# Subset data for individuals with BMI >= 37 and cardio = 1
df_bmi37_cardio <- subset(df_bmi37, cardio == 1)
# Calculate the percentage
percentage_bmi37_cardio <- round((nrow(df_bmi37_cardio) / nrow(df_bmi37)) * 100, 2)
# Print the result
cat("Percentage of individuals with BMI >= 37 and have cardiovascular:", percentage_bmi37_cardio, "%\n")

# Subset data for 'bp_cat' equal to 'high 3'
df_high3 <- filter(cardio_data, bp_category == 'high 3')
# Subset data for 'bp_cat' equal to 'high 3' and 'cardio' equal to 1
df_high_cardio <- filter(df_high3, cardio == 1)
# Calculate the percentage
percentage_high_cardio <- round((nrow(df_high_cardio) / nrow(df_high3)) * 100, 2)
# Print the result
cat("Percentage of individuals with 'high 3' blood pressure and who is diagnosed with cardiovascular:", percentage_high_cardio, "%\n")

# Subset data for 'alco' equal to 1 or 'smoke' equal to 1
df_cohol_smoke <- filter(cardio_data, alco == 1 | smoke == 1)
# Calculate the percentage
percentage_cohol_smoke <- round((nrow(df_cohol_smoke) / nrow(cardio_data)) * 100, 2)
# Print the result
cat("Percentage of individuals with consume alcohol or smokes:", percentage_cohol_smoke, "%\n")

df_cohol_smoke_cardio <- filter(df_cohol_smoke, cardio == 1)
# Calculate the percentage
percentage_cohol_smoke_cardio <- round((nrow(df_cohol_smoke_cardio) / nrow(df_cohol_smoke)) * 100, 2)
# Print the result
cat("Percentage of individuals who drinks alcohol or smokes and have cardiovascular:", percentage_cohol_smoke_cardio, "%\n")


# Subset data for 'active' equal to 0
df_not_active <- filter(cardio_data, active == 0)
# Subset data for individuals with 'cardio' equal to 1
df_not_active_cardio <- filter(df_not_active, cardio == 1)
# Calculate the percentage
percentage_not_active_cardio <- round((nrow(df_not_active_cardio) / nrow(df_not_active)) * 100, 2)
# Print the result
cat("Percentage of individuals who are not active and have cardiovascular:", percentage_not_active_cardio, "%\n")

head(cardio_data)
#Correlation
#numeric_variables <- cardio_data[, c("cholesterol","gluc","smoke","alco","active","years","bmi","cardio")]
numeric_variables <- cardio_data[, c("gender","height","weight","cholesterol","ap_hi","ap_lo","gluc","smoke","alco","active","years","bmi","cardio")]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_variables)

# Print the correlation matrix
print(correlation_matrix)

corrplot(correlation_matrix, method = "color", type = "lower", addCoef.col = "black", tl.col = "black")

cardio_data <- cardio_data %>%
  mutate(bp_category_numeric = case_when(
    bp_category == "normal"   ~ 1,
    bp_category == "elevated" ~ 2,
    bp_category == "high 1"   ~ 3,
    bp_category == "high 2"   ~ 4,
    bp_category == "high 3"   ~ 5,
    TRUE                      ~ NA
  ))
smoke_alc_bp <- cardio_data[, c("smoke","alco","bp_category_numeric")]
cor(smoke_alc_bp)
corrplot(cor(smoke_alc_bp), method = "color", type = "upper", addCoef.col = "black", tl.col = "black")

##Hypothesis Testing
#Comparison of BMI for Active vs. Inactive individuals

# Subset data for active and inactive individuals
active_data <- cardio_data %>% filter(active == 1)
inactive_data <- cardio_data %>% filter(active == 0)

# Perform independent samples t-test
result <- t.test(active_data$bmi, inactive_data$bmi)
result

#Comparison of Proportions (Smoking between Genders)
smoke_gender_table <- table(cardio_data$smoke, cardio_data$gender)

# Perform a chi-square test for independence
result <- chisq.test(smoke_gender_table)

# Display the results
result

#Correlation Hypothesis (Systolic and Diastolic Blood Pressure)
result <- cor.test(cardio_data$ap_hi, cardio_data$ap_lo)

# Display the results
result
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

#Linear Regression
set.seed(123)  # Set seed for reproducibility
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))

# Split the data into training and testing sets
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]
# Perform linear regression on the training data
#model <- lm(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years + bmi, data = train_data)
model <- lm(cardio ~ cholesterol + gluc + active + years + bmi + ap_hi + ap_lo, data = train_data)
# Display summary of the linear regression model
summary(model)
# Make predictions on the testing data
predictions <- predict(model, newdata = test_data)

# Evaluate the model performance (you can use different metrics)
# mean squared error (MSE)
mse <- mean((test_data$cardio - predictions)^2)
percentage_mse <- round((mse / mean(test_data$cardio^2) * 100),2) # Calculate percentage MSE
print(paste("Mean Squared Error (as a percentage):", percentage_mse))

#Random Forest Classification
set.seed(123)  # Set seed for reproducibility
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))

# Split the data into training and testing sets
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]

train_data$cardio <- as.factor(train_data$cardio)
test_data$cardio <- as.factor(test_data$cardio)
# Perform Random Forest classification on the training data
model_rf <- randomForest(cardio ~ cholesterol + gluc + active + years + bmi + ap_hi + ap_lo, data = train_data)

# Make predictions on the testing data
predictions_rf <- predict(model_rf, newdata = test_data)
# Evaluate the model performance
conf_matrix <- table(predictions_rf, test_data$cardio)
print("Confusion Matrix:")
print(conf_matrix)
# Calculate accuracy, sensitivity, specificity, etc.
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
print(paste("Accuracy:", round(accuracy * 100,2)))

##Decision Tree
set.seed(123)  # Set seed for reproducibility

# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
cardio_data_filtered_scaled$cardio <- as.factor(cardio_data_filtered_scaled$cardio)
# Split the data into training and testing sets
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]

# Build a decision tree model
#model_tree <- tree(cardio ~ gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + years + bmi, data = train_data)
model_tree <- tree(cardio ~ cholesterol + gluc + active + years + bmi + ap_hi + ap_lo, data = train_data)

# Make predictions on the testing data
predictions_tree <- predict(model_tree, newdata = test_data, type = "class")
# Evaluate the model performance
conf_matrix_tree <- table(predictions_tree, test_data$cardio)
print("Confusion Matrix:")
print(conf_matrix_tree)
# Calculate accuracy, sensitivity, specificity, etc.
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
accuracy_tree
print(paste("Accuracy:", round(accuracy_tree*100,2)))

#XGBOOST Classification
set.seed(123)  # Set seed for reproducibility

selected_features <- c("cholesterol", "gluc", "active", "years", "bmi", "ap_hi", "ap_lo", "cardio")
cardio_data_filtered_scaled <- cardio_data_filtered_scaled[selected_features]
# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
cardio_data_filtered_scaled$cardio <- cardio_data$cardio
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
confusionMatrix(factor(predictions_xgb_class), factor(test_data$cardio))
# Compare predicted labels to actual labels
accuracy <- sum(predictions_xgb_class == test_data$cardio) / length(test_data$cardio)

print(paste("Accuracy:", round(accuracy*100 ,2)))

##Naive Bayes Classifier
set.seed(123)  # Set seed for reproducibility

# Convert 'cardio' to a factor with two levels (assuming 0 and 1)
cardio_data_filtered_scaled$cardio <- as.factor(cardio_data_filtered_scaled$cardio)

# Split the data into training and testing sets
split_index <- sample(seq_len(nrow(cardio_data_filtered_scaled)), 0.8 * nrow(cardio_data_filtered_scaled))
train_data <- cardio_data_filtered_scaled[split_index, ]
test_data <- cardio_data_filtered_scaled[-split_index, ]
# Define the features used for training
features <- c( "ap_hi", "ap_lo", "cholesterol", "gluc", "active", "years", "bmi")

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

print(paste("Accuracy:", round(accuracy_nb*100,2)))

head(cardio_data)
