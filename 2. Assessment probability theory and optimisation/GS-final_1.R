
#Q2A
library(datarium)
data(marketing)
View(marketing)

# Extract the variables from the dataset
Y <- marketing$sales
X1 <- marketing$youtube
X2 <- marketing$newspaper
X3 <- marketing$facebook

# Calculate β_hat using equation (4)
X <- cbind(1, X1, X2, X3)  # Add a column of 1s for β0
X
n <- length(Y)
beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% Y)

# Calculate s^2 and s.e(β) using equations (5)
Y_hat <- X %*% beta_hat
residuals <- Y - Y_hat
s_squared <- sum(residuals^2) / (n - 4)
se_beta <- sqrt(s_squared * diag(solve(t(X) %*% X)))

# Print the results
beta_hat 
se_beta   


##Q2B
# Fit a multiple linear regression model using lm()
lm_model <- lm(sales ~ youtube + newspaper + facebook, data = marketing)
summary(lm_model)

# Extract the coefficients and their standard errors
lm_coeffs <- coef(lm_model)
lm_se <- summary(lm_model)$coefficients[, "Std. Error"]

# Print the coefficients and standard errors from lm()
lm_coeffs
lm_se


###
#Q3B
# Gradient Descent function
gradient_descent <- function(X, Y, learning_rate, max_iterations, tolerance) {
  n <- length(Y)
  beta <- rep(0, ncol(X))  # Initialize beta
  for (i in 1:max_iterations) {
    # Calculate the gradient
      gradient = (2/n) * (t(X) %*% (X %*% beta - Y))
      #print(gradient)
    # Check for missing or NaN values in the gradient
    if (any(is.na(gradient)) || any(is.nan(gradient))) {
      cat("Gradient contains missing or NaN values. Exiting...\n")
      break
    }
    # Update beta
    beta = beta - (learning_rate * gradient)
    
    # Calculate the norm of the gradient
    gradient_norm <- sqrt(sum(gradient^2))
    
    # Check for convergence
    if (gradient_norm < tolerance) {
      cat("Gradient Descent converged after", i, "iterations.\n")
      break
    }
  }
  
  return(beta)
}



marketing_1 = as.data.frame(scale(marketing))
#marketing_1

Y <- marketing$sales
X1 <- marketing_1$youtube
X2 <- marketing_1$facebook
X3 <- marketing_1$newspaper

X = cbind(1, X1, X2, X3)

# Set hyperparameters
learning_rate <- .000005
max_iterations <- 10000000
tolerance <- 1e-4
# Use Gradient Descent to estimate beta
beta_gradient_descent <- gradient_descent(X, Y, learning_rate, max_iterations, tolerance)
# Print the results
beta_gradient_descent




