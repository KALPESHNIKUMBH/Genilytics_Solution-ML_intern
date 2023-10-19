# Load your dataset
data <- read.csv("C:/Users/kalpe/Desktop/Genilytics_Solution-ML_intern/7. Unemployment Rate Prediction/Aus_data_2023.csv")
str(data)

##

# Check for missing values in the dataset
missing_values <- sapply(data, function(x) sum(is.na(x)))
 
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


############################################################################################

NN

############################################################################################


# Load the neuralnet library
library(neuralnet)

# Set a seed for reproducibility
set.seed(123)

# Split the data into a training set and a test set
set.seed(123)  # You can change the seed for randomness
sample_size <- floor(0.7 * nrow(data))  # 70% for training, 30% for testing
train_indices <- sample(1:nrow(data), sample_size)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define the neural network model
nn_model <- neuralnet(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, 
                      data = train_data, 
                      hidden = c(5, 3),  # Specify the number of hidden layers and neurons
                      linear.output = TRUE)  # For regression tasks

# Make predictions on the test set
predicted_values_test_nn <- predict(nn_model, newdata = test_data)

# Extract the target variable from the test set
target_variable_test <- test_data$Y

# Calculate Mean Absolute Error (MAE) on the test set
mae_test_nn <- mean(abs(predicted_values_test_nn - target_variable_test))

# Calculate Mean Squared Error (MSE) on the test set
mse_test_nn <- mean((predicted_values_test_nn - target_variable_test)^2)

# Calculate Root Mean Squared Error (RMSE) on the test set
rmse_test_nn <- sqrt(mse_test_nn)

# Calculate R-squared (coefficient of determination) on the test set
ssr_nn <- sum((target_variable_test - predicted_values_test_nn)^2)
sst <- sum((target_variable_test - mean(target_variable_test))^2)
rsq_test_nn <- 1 - (ssr_nn / sst)

cat("Mean Absolute Error (MAE) for Neural Network on the test set:", mae_test_nn, "\n")
cat("Mean Squared Error (MSE) for Neural Network on the test set:", mse_test_nn, "\n")
cat("Root Mean Squared Error (RMSE) for Neural Network on the test set:", rmse_test_nn, "\n")
cat("R-squared (R^2) for Neural Network on the test set:", rsq_test_nn, "\n")

###########################################################################################

#Hyperparaameter Tuning

###########################################################################################
# Load the necessary libraries
library(neuralnet)

# Set a seed for reproducibility
set.seed(123)

# Split the data into a training set and a test set
set.seed(123)  # You can change the seed for randomness
sample_size <- floor(0.7 * nrow(data))  # 70% for training, 30% for testing
train_indices <- sample(1:nrow(data), sample_size)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create a function to build and train the neural network
train_neural_network <- function(hidden_layers, neurons) {
  nn_model <- neuralnet(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, 
                        data = train_data, 
                        hidden = c(hidden_layers, neurons),  # Specify hidden layers and neurons
                        linear.output = TRUE)  # For regression tasks
  
  # Make predictions on the test set
  predicted_values_test_nn <- predict(nn_model, newdata = test_data)
  
  # Calculate Mean Absolute Error (MAE) on the test set
  mae_test_nn <- mean(abs(predicted_values_test_nn - test_data$Y))
  
  # Calculate Mean Squared Error (MSE) on the test set
  mse_test_nn <- mean((predicted_values_test_nn - test_data$Y)^2)
  
  # Calculate Root Mean Squared Error (RMSE) on the test set
  rmse_test_nn <- sqrt(mse_test_nn)
  
  # Calculate R-squared (R^2) on the test set
  ssr_nn <- sum((test_data$Y - predicted_values_test_nn)^2)
  sst <- sum((test_data$Y - mean(test_data$Y))^2)
  rsq_test_nn <- 1 - (ssr_nn / sst)
  
  return(list(model = nn_model, mae = mae_test_nn, mse = mse_test_nn, rmse = rmse_test_nn, rsq = rsq_test_nn))
}

# Define hyperparameter grid to search
hidden_layer_grid <- c(1, 2, 3)  # Test different numbers of hidden layers
neurons_grid <- c(3, 5, 7)     # Test different numbers of neurons

best_mae <- Inf  # Initialize with a high value
best_model <- NULL
best_mse <- Inf
best_rmse <- Inf
best_rsq <- -Inf

# Perform hyperparameter tuning
for (hidden_layers in hidden_layer_grid) {
  for (neurons in neurons_grid) {
    # Train the model with the current hyperparameters
    result <- train_neural_network(hidden_layers, neurons)
    
    # Check if the current model has a lower MAE
    if (result$mae < best_mae) {
      best_mae <- result$mae
      best_mse <- result$mse
      best_rmse <- result$rmse
      best_rsq <- result$rsq
      best_model <- result$model
      best_hidden_layers <- hidden_layers
      best_neurons <- neurons
    }
  }
}

cat("Best Hyperparameters:\n")
cat("Hidden Layers:", best_hidden_layers, "\n")
cat("Neurons per Layer:", best_neurons, "\n")
cat("Best Mean Absolute Error (MAE):", best_mae, "\n")
cat("Best Mean Squared Error (MSE):", best_mse, "\n")
cat("Best Root Mean Squared Error (RMSE):", best_rmse, "\n")
cat("Best R-squared (R^2):", best_rsq, "\n")

# Now you can use the best_model for predictions and evaluation
