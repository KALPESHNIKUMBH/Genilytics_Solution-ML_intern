# Load necessary libraries
library(readxl)
library(randomForest)
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

# Split the data into training (70%) and test (30%) sets
set.seed(123)  # Set a random seed for reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define the Random Forest model
rf_model <- randomForest(`default payment next month` ~ ., data = train_data, ntree = 100)

summary(rf_model)
# Make predictions on the training data
train_predictions <- predict(rf_model, train_data)

# Calculate training accuracy
train_accuracy <- mean(train_predictions == train_data$`default payment next month`)
cat("Training Accuracy:", train_accuracy, "\n")

# Make predictions on the test data
test_predictions <- predict(rf_model, test_data)

# Create a confusion matrix
conf_matrix <- confusionMatrix(test_predictions, test_data$`default payment next month`)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate testing accuracy
test_accuracy <- conf_matrix$overall[1]
cat("Testing Accuracy:", test_accuracy, "\n")

# Calculate recall (sensitivity)
recall <- conf_matrix$byClass["Sensitivity"]
cat("Recall (Sensitivity):", recall, "\n")

# Calculate AUC (Area Under the ROC Curve)
roc_obj <- roc(test_data$`default payment next month`, as.numeric(test_predictions))
auc <- auc(roc_obj)
cat("AUC (Area Under the ROC Curve):", auc, "\n")

# Plot the ROC curve
roc_curve <- roc(test_data$`default payment next month`, as.numeric(test_predictions))
plot(roc_curve, main = "ROC Curve", auc.polygon = TRUE, print.auc = TRUE)
