# Load necessary libraries
library(readxl)
library(caret)
library(pROC)

# Specify the file path with double backslashes
file_path <- "C:\\Users\\admin\\Desktop\\Genilytics Solutions\\Genilytics_Solution-ML_intern\\5. Credit default modeling\\Instructions and Data\\CreditCard_Data.xls"


# Load the dataset using the correct file path
data <- read_excel(file_path)

# Check for missing values
missing_values <- colSums(is.na(data))

# Print columns with missing values
print(missing_values[missing_values > 0])

# Example: Remove rows with missing values
data <- data[complete.cases(data), ]

# Set the first row as column names and remove it
colnames(data) <- data[1, ]
data <- data[-1, ]

# Ensure that AGE is numeric
data$AGE <- as.numeric(data$AGE)

# Calculate z-scores for AGE
z_scores <- scale(data$AGE)

# Remove rows with AGE values outside a certain range (e.g., within 3 standard deviations)
data <- data[abs(z_scores) < 3, ]

# Convert relevant columns to appropriate data types
data$ID <- as.numeric(data$ID)
data$LIMIT_BAL <- as.numeric(data$LIMIT_BAL)
data$SEX <- as.numeric(data$SEX)
data$EDUCATION <- as.numeric(data$EDUCATION)
data$MARRIAGE <- as.numeric(data$MARRIAGE)
data$PAY_0 <- as.numeric(data$PAY_0)
data$PAY_2 <- as.numeric(data$PAY_2)
data$PAY_3 <- as.numeric(data$PAY_3)
data$PAY_4 <- as.numeric(data$PAY_4)
data$PAY_5 <- as.numeric(data$PAY_5)
data$PAY_6 <- as.numeric(data$PAY_6)
data$BILL_AMT1 <- as.numeric(data$BILL_AMT1)
data$BILL_AMT2 <- as.numeric(data$BILL_AMT2)
data$BILL_AMT3 <- as.numeric(data$BILL_AMT3)
data$BILL_AMT4 <- as.numeric(data$BILL_AMT4)
data$BILL_AMT5 <- as.numeric(data$BILL_AMT5)
data$BILL_AMT6 <- as.numeric(data$BILL_AMT6)
data$PAY_AMT1 <- as.numeric(data$PAY_AMT1)
data$PAY_AMT2 <- as.numeric(data$PAY_AMT2)
data$PAY_AMT3 <- as.numeric(data$PAY_AMT3)
data$PAY_AMT4 <- as.numeric(data$PAY_AMT4)
data$PAY_AMT5 <- as.numeric(data$PAY_AMT5)
data$PAY_AMT6 <- as.numeric(data$PAY_AMT6)

# Convert the response variable to a binary factor variable (0 or 1)
data$`default payment next month` <- as.factor(data$`default payment next month`)

str(data)

###
library(ggplot2)
ggplot(data, aes(x = `default payment next month`, fill = `default payment next month`)) +
  geom_bar() +
  labs(title = "Distribution of Default Payment")
##
ggplot(data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution")
##
ggplot(data, aes(x = EDUCATION, fill = `default payment next month`)) +
  geom_bar(position = "dodge") +
  labs(title = "Default Status by Education Level", x = "Education Level")
##
ggplot(data, aes(x = AGE, y = LIMIT_BAL)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Credit Limit")
##

###
#Split the data into training (70%) and test (30%) sets
set.seed(123)  # Set a random seed for reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# Dimensions of the training data
train_dimensions <- dim(train_data)
cat("Dimensions of the training data (rows x columns):\n")
cat("Rows:", train_dimensions[1], "\n")
cat("Columns:", train_dimensions[2], "\n")

# Dimensions of the test data
test_dimensions <- dim(test_data)
cat("\nDimensions of the test data (rows x columns):\n")
cat("Rows:", test_dimensions[1], "\n")
cat("Columns:", test_dimensions[2], "\n")


