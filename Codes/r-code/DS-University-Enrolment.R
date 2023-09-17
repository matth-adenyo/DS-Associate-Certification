
#Load required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(tidyr)
library(glmnet)
library(lattice)
library(e1071)



#Read datasets
data <- read.csv("D:/Data Science Projects/DS-Associate-Certification/Data/university_enrollment_2306.csv")

# Display data summary
str(data)
dim(data)
sapply(data, class)
summary(data)



#TASK 1:
# Compare the description for each column

# course_id Data Validation
unique_id <- length(unique(data$course_id)) == nrow(data)
course_count_na <- sum(is.na(data$course_id))

cat("----- course_id -----\n")
cat(paste("Column 'course_id' has only unique values:", ifelse(unique_id, "TRUE", "FALSE"), "\n"))
cat(paste("Sum of missing values:", course_count_na, "\n\n"))

# course_type Data Validation
online_class <- unique(data$course_type)
course_type_count_na <- sum(is.na(data$course_type))

cat("----- course_type -----\n")
cat("Values in this column:\n", online_class, "\n")
cat(paste("Sum of missing values:", course_type_count_na, "\n\n"))

# year Data Validation
years <- unique(data$year)
year_count_na <- sum(is.na(data$year))

cat("----- year -----\n")
cat("Values in this column:\n", years, "\n")
cat(paste("Sum of missing values:", year_count_na, "\n\n"))

# enrollment_count Data Validation
enrollment_count_na <- sum(is.na(data$enrollment_count))

cat("----- enrollment_count -----\n")
cat("Description of values in this column:\n")
summary(data$enrollment_count)
cat(paste("Sum of missing values:", enrollment_count_na, "\n\n"))

# pre_score Data Validation
# Replace non-numeric values with NA
data$pre_score[data$pre_score == "-"] <- NA
pre_score_count_na <- sum(is.na(data$pre_score))

cat("----- pre_score -----\n")
cat("Description of values in this column:\n")
summary(as.numeric(data$pre_score))
cat(paste("Sum of missing values before cleaning:", pre_score_count_na, "\n\n"))

# post_score Data Validation
post_score_count_na <- sum(is.na(data$post_score))

cat("----- post_score -----\n")
cat("Description of values in this column:\n")
summary(data$post_score)
cat(paste("Sum of missing values before cleaning:", post_score_count_na, "\n\n"))

# pre_requirement Data Validation
pre_req <- unique(data$pre_requirement)
pre_req_count_na <- sum(is.na(data$pre_requirement))

cat("----- pre_requirement -----\n")
cat("Values in this column:", pre_req, "\n")
cat(paste("Sum of missing values before cleaning:", pre_req_count_na, "\n\n"))

# department Data Validation
dep <- unique(data$department)
dep_count_na <- sum(is.na(data$department))

cat("----- department -----\n")
cat("Values in this column:\n", dep, "\n")
cat(paste("Sum of missing values before cleaning:", dep_count_na, "\n\n"))


# Data Cleaning
cat("For each column that presented missing values, or incorrect description:\n")
# pre_score - replace missing values and "-"
data$pre_score[is.na(data$pre_score) | data$pre_score == "-"] <- 0
pre_score_count_na <- sum(is.na(data$pre_score))
cat("----- pre_score -----\n")
cat(paste("Sum of missing values after cleaning:", pre_score_count_na, "\n\n"))

# post_score - replace missing values and "-"
data$post_score[is.na(data$post_score) | data$post_score == "-"] <- 0
post_score_count_na <- sum(is.na(data$post_score))
cat("----- post_score -----\n")
cat(paste("Sum of missing values after cleaning:", post_score_count_na, "\n\n"))

# pre_requirement - replace missing values and "-"
data$pre_requirement[is.na(data$pre_requirement) | data$pre_requirement == "-"] <- "None"
pre_req_count_na <- sum(is.na(data$pre_requirement))
cat("----- pre_requirement -----\n")
cat(paste("Sum of missing values after cleaning:", pre_req_count_na), "\n")
cat(paste("Unique values after cleaning:", unique(data$pre_requirement), "\n\n"))

# department - replace "Math" values with "Mathematics"
data$department <- ifelse(data$department == "Math", "Mathematics", data$department)
cat("----- department -----\n")
cat(paste("Unique values after cleaning:", unique(data$department), "\n"))




#TASK 2:
# Creating subplots
par(mfrow=c(1, 2), mar=c(4, 4, 2, 1))

# Distribution of Enrollment Count
ggplot(data, aes(x = enrollment_count)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  # geom_density(alpha = 0.5, color = "red") +
  labs(title = "Distribution of Enrollment Counts") +
  theme_minimal()


# Boxplot of Enrollment Count
ggplot(data, aes(y = enrollment_count)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Enrollment Counts") +
  theme_minimal()

# Reset the plotting layout
par(mfrow=c(1, 1))

# Calculate important metrics
mean_val <- mean(data$enrollment_count)
median_val <- median(data$enrollment_count)
mode_val <- as.numeric(names(table(data$enrollment_count))[table(data$enrollment_count)
                                                           == max(table(data$enrollment_count))])

cat(paste("- Mean:", mean_val, "\n"))
cat(paste("- Median:", median_val, "\n"))
cat(paste("- Mode:", mode_val, "\n"))



#TASK 3
#Bar plot to Visualize the Course type
ggplot(data, aes(x = course_type, fill = course_type)) +
  geom_bar() +
  labs(title = "Number of Courses by Type") +
  theme_minimal() +
  theme(legend.position = "none")

# Calculate and print the proportion
classroom <- sum(data$course_type == "classroom")
online <- nrow(data) - classroom
proportion <- round((online / (online + classroom)) * 100)

cat(paste("- Proportion of Online to Classroom Type of Courses:", proportion, "%\n"))



#TASK 4:
# Violin plot
ggplot(data, aes(x = course_type, y = enrollment_count)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.5) +
  labs(title = "Enrollment Count by Course Type") +
  theme_minimal()

# Calculate and print the means
classroom <- round(mean(data[data$course_type == "classroom", "enrollment_count"], na.rm = TRUE))
online <- round(mean(data[data$course_type == "online", "enrollment_count"], na.rm = TRUE))

cat(paste("- Avg. enrollment count for Classroom Courses:", format(classroom, big.mark = ","), "\n"))
cat(paste("- Avg. enrollment count for Online Courses:", format(online, big.mark = ","), "\n"))




#TASK 6:
#converting all categorical variables to factors and apply OneHotEncoder

# Convert 'pre_score' to a factor variable
data$pre_score <- as.factor(data$pre_score)

# Apply one-hot encoding to categorical variables
data <- data %>%
  dplyr::mutate(across(pre_score, ~ as.integer(.)))

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
splitIndex <- createDataPartition(data$enrollment_count, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]



# Fit a Linear Regression model (LM)
lm_model <- lm(enrollment_count ~ ., data = train_data)

# Make predictions using the LM model
predictions <- predict(lm_model, newdata = test_data)

# Calculate Root Mean Squared Error (RMSE)
rmse_lm <- sqrt(mean((predictions - test_data$enrollment_count)^2))
cat("Root Mean Squared Error (LM):", rmse_lm, "\n")

# Create a data frame for plotting
plot_data <- data.frame(
  actual = test_data$enrollment_count,
  predicted = predictions
)

# Create a scatter plot comparing actual and LM predicted values
ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Enrollment", y = "LM Predicted Enrollment",
       title = "Actual vs. LM Predicted Enrollment") +
  annotate("text", x = max(plot_data$actual), y = min(plot_data$predicted),
           label = paste("RMSE =", round(rmse_lm, 2)), hjust = 1)





#TASK 7:
# Fit a Support Vector Machine (SVM) model
svm_model <- svm(enrollment_count ~ ., data = train_data)

# Make predictions using the SVM model
predictions <- predict(svm_model, newdata = test_data)

# Calculate Root Mean Squared Error (RMSE)
rmse_svm <- sqrt(mean((predictions - test_data$enrollment_count)^2))
cat("Root Mean Squared Error (SVM):", rmse_svm, "\n")

# Create a data frame for plotting
plot_data <- data.frame(
  actual = test_data$enrollment_count,
  predicted = predictions
)

# Create a scatter plot comparing actual and SVM predicted values
ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Enrollment", y = "SVM Predicted Enrollment",
       title = "Actual vs. SVM Predicted Enrollment") +
  annotate("text", x = max(plot_data$actual), y = min(plot_data$predicted),
           label = paste("RMSE =", round(rmse_svm, 2)), hjust = 1)