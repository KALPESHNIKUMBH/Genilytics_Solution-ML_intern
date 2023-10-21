
# Load your dataset
data <- read.csv("C:/Users/kalpe/Desktop/Genilytics_Solution-ML_intern/7. Unemployment Rate Prediction/Aus_data_2023.csv")
str(data)
#data
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
str(data)

##############################################################################

standardize and normalize

##############################################################################
# Identify columns to standardize and normalize
columns_to_standardize <- c("X1", "X2", "X3", "X4", "X6")
columns_to_normalize <- c("X5", "X7")

# Standardization (Z-score normalization) for selected columns
data[, columns_to_standardize] <- scale(data[, columns_to_standardize])

# Normalization (Min-Max scaling) for selected columns
for (col in columns_to_normalize) {
  data[[col]] <- (data[[col]] - min(data[[col]])) / (max(data[[col]]) - min(data[[col]]))
}

# Check the resulting dataset
str(data)


########################################################################################

#Mean,SD,MIN,MAX

########################################################################################
# Calculate mean for each numeric variable
mean_values <- sapply(data[, 2:9], mean)


# Calculate minimum for each numeric variable
min_values <- sapply(data[, 2:9], min)

# Calculate maximum for each numeric variable
max_values <- sapply(data[, 2:9], max)

summary_stats <- data.frame(
  Variable = names(data)[2:9],
  Mean = mean_values,
    Minimum = min_values,
  Maximum = max_values
)

print(summary_stats)

########################################################################################

Visualization

########################################################################################
library(corrplot)
# Calculate and visualize correlations
cor_matrix <- cor(data[, c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7")])
corrplot(cor_matrix, method = "color")

 
#####################################################################################
 
# Convert Year_Q to a Date object
data$Year_Q <- as.Date(paste0("01-", gsub("_", " ", data$Year_Q)), format = "%d-%Y %b")
 
# Time series plot of the unemployment rate
plot(data$Year_Q, data$Y, type = "l", xlab = "Year", ylab = "Unemployment Rate", main = "Unemployment Rate Over Time")
 
#####################################################################################

# Sort the data by employment rate (variable Y) in descending order and select the top 3 years
top_years <- head(data[order(-data$Y), ], 3)

# Extract unique year and quarter combinations
unique_years <- unique(top_years$Year_Q)

# Create a bar chart
barplot(top_years$Y, names.arg = unique_years, 
        main = "Top 3 Years with Highest Unemployment Rate",
        xlab = "Year and Quarter",
        ylab = "Employment Rate",
        col = "skyblue", 
        border = "black",
        ylim = c(0, max(top_years$Y) + 1))

# Add percentage labels on top of each bar
percentage_labels <- paste0(round(top_years$Y, 2), "%")
text(1:3, top_years$Y, labels = percentage_labels, pos = 3, cex = 1.2)

####################################################################################

# Sort the data by employment rate (variable Y) in ascending order and select the lowest 3 years
lowest_years <- head(data[order(data$Y), ], 3)

# Extract unique year and quarter combinations
unique_years <- unique(lowest_years$Year_Q)

# Create a bar chart
barplot(lowest_years$Y, names.arg = unique_years, 
        main = "Bottom 3 Years with Lowest Unemployment Rate",
        xlab = "Year and Quarter",
        ylab = "Employment Rate",
        col = "skyblue", 
        border = "black",
        ylim = c(0, max(lowest_years$Y) + 1))

# Add percentage labels on top of each bar
percentage_labels <- paste0(round(lowest_years$Y, 2), "%")
text(1:3, lowest_years$Y, labels = percentage_labels, pos = 3, cex = 1.2)
