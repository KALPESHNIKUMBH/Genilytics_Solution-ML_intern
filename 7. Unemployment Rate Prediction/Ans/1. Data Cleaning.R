# Load your dataset
data <- read.csv("//Aus_data_2023.csv")
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

##########
 
# Calculate and visualize correlations
cor_matrix <- cor(data[, c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7")])
corrplot(cor_matrix, method = "color")
 
#######
 
# Convert Year_Q to a Date object
data$Year_Q <- as.Date(paste0("01-", gsub("_", " ", data$Year_Q)), format = "%d-%Y %b")
 
# Time series plot of the unemployment rate
plot(data$Year_Q, data$Y, type = "l", xlab = "Year", ylab = "Unemployment Rate", main = "Unemployment Rate Over Time")
 
#######
