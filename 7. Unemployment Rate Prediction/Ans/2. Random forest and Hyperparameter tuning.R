# Load your dataset
data <- read.csv("C:/Users/kalpe/Desktop/Genilytics_Solution-ML_intern/7. Unemployment Rate Prediction/Aus_data_2023.csv")
str(data)
#data
##

# Check for missing values in the dataset
missing_values <- sapply(data, function(x) sum(is.na(x)))
#data 
# Find the columns with missing values
columns_with_na <- names(data)[missing_values > 0]
 
# Replace missing values with the mean of their respective columns
for (col in columns_with_na) {
mean_value <- mean(data[[col]], na.rm = TRUE) # Calculate the mean, excluding NAs
data[[col]] <- ifelse(is.na(data[[col]]), mean_value, data[[col]])
}
 
# Check if missing values are replaced
missing_values_updated <- sapply(data, function(x) sum(is.na(x)))
missing_values_updated
data






#############################################################################

#Random_Forest

#############################################################################



# Load any required libraries
library(randomForest)
library(caret)

# Set a seed for reproducibility
set.seed(123)

# Assuming you've already loaded and preprocessed your data as in the previous code
# Perform a train-test split
train_indices <- createDataPartition(data$Y, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define your target variable for training and testing
target_variable_train <- train_data$Y
target_variable_test <- test_data$Y

# Define the predictor variables for training and testing
predictor_variables_train <- train_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")]
predictor_variables_test <- test_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")]

# Create the Random Forest Regression model
rf_model <- randomForest(predictor_variables_train, target_variable_train, ntree = 100)

# Make predictions on the test set
predicted_values_test <- predict(rf_model, predictor_variables_test)

# Calculate Mean Absolute Error (MAE) on the test set
mae_test <- mean(abs(predicted_values_test - target_variable_test))

# Calculate Mean Squared Error (MSE) on the test set
mse_test <- mean((predicted_values_test - target_variable_test)^2)

# Calculate Root Mean Squared Error (RMSE) on the test set
rmse_test <- sqrt(mse_test)

# Calculate R-squared (coefficient of determination) on the test set
sst <- sum((target_variable_test - mean(target_variable_test))^2)
ssr <- sum((target_variable_test - predicted_values_test)^2)
rsq_test <- 1 - (ssr / sst)

cat("Mean Absolute Error (MAE) on the test set:", mae_test, "\n")
cat("Mean Squared Error (MSE) on the test set:", mse_test, "\n")
cat("Root Mean Squared Error (RMSE) on the test set:", rmse_test, "\n")
cat("R-squared (R^2) on the test set:", rsq_test, "\n")







#############################################################################

#Hyperparameter Tuning ( currently working)

#############################################################################
# Load necessary libraries
library(randomForest)
library(caret)

# Set a seed for reproducibility
set.seed(123)

# Assuming you've already loaded and preprocessed your data as in the previous code
# Perform a train-test split
train_indices <- createDataPartition(data$Y, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define your target variable for training and testing
target_variable_train <- train_data$Y
target_variable_test <- test_data$Y

# Define the predictor variables for training and testing
predictor_variables_train <- train_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")]
predictor_variables_test <- test_data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")]

# Define a hyperparameter grid
hyperparameter_grid <- expand.grid(
    ntree = c(50, 100, 200),
    mtry = c(2, 3, 4)
)

# Create a control object for cross-validation
control <- trainControl(method = "cv", number = 5)

# Create a data frame to store results
results <- data.frame()

# Loop through the hyperparameter grid
for (i in 1:nrow(hyperparameter_grid)) {
    ntree <- hyperparameter_grid$ntree[i]
    mtry <- hyperparameter_grid$mtry[i]
    
    # Train a Random Forest model
    model <- train(
        Y ~ .,
        data = train_data,
        method = "rf",
        trControl = control,
        tuneGrid = data.frame(mtry = mtry),
        ntree = ntree
    )

    # Evaluate the model and store the results
    mae <- min(model$results$MAE)
    mse <- min(model$results$RMSE)
    rmse <- min(model$results$RMSE)
    rsq <- max(model$results$Rsquared)

    results <- rbind(results, data.frame(ntree = ntree, mtry = mtry, MAE = mae, MSE = mse, RMSE = rmse, Rsquared = rsq))
}

# Select the best hyperparameters based on RMSE or R-squared
best_params <- results[which.min(results$RMSE), ]

# Train the final Random Forest model with the best hyperparameters
final_rf_model <- randomForest(
    predictor_variables_train,
    target_variable_train,
    ntree = best_params$ntree,
    mtry = best_params$mtry
)

# Make predictions on the test set
predicted_values_test <- predict(final_rf_model, predictor_variables_test)

# Calculate Mean Absolute Error (MAE) on the test set
mae_test <- mean(abs(predicted_values_test - target_variable_test))

# Calculate Mean Squared Error (MSE) on the test set
mse_test <- mean((predicted_values_test - target_variable_test)^2)

# Calculate Root Mean Squared Error (RMSE) on the test set
rmse_test <- sqrt(mse_test)

# Calculate R-squared (coefficient of determination) on the test set
sst <- sum((target_variable_test - mean(target_variable_test))^2)
ssr <- sum((target_variable_test - predicted_values_test)^2)
rsq_test <- 1 - (ssr / sst)

cat("Best Hyperparameters: ntree =", best_params$ntree, "mtry =", best_params$mtry, "\n")
cat("Mean Absolute Error (MAE) on the test set:", mae_test, "\n")
cat("Mean Squared Error (MSE) on the test set:", mse_test, "\n")
cat("Root Mean Squared Error (RMSE) on the test set:", rmse_test, "\n")
cat("R-squared (R^2) on the test set:", rsq_test, "\n")

