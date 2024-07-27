# Set path
setwd("E:/Academic S7/MA4014 - Linear Models and Multivariate Statistics/Linear-Models")
current_path <- getwd()

# Read the data from the text file
data <- read.table("ciggerate.txt", header = TRUE, sep = ",")
print(data)

# Check the structure of the data
str(data)

# Convert all columns to numeric, except for the first one which is the State name
data[, -1] <- lapply(data[, -1], function(x) as.numeric(as.character(x)))

# Recheck the structure of the data to confirm the conversion
str(data)

# Calculate correlation matrix
correlation_matrix <- cor(data[, -1], use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix:\n")
print(correlation_matrix)

# Summary of the data
data_summary <- summary(data)

# Print summary
cat("Data Summary:\n")
print(data_summary)

# Additional statistical analysis
# Calculate mean, median, and standard deviation for each column
means <- colMeans(data[, -1], na.rm = TRUE)
medians <- apply(data[, -1], 2, median, na.rm = TRUE)
sds <- apply(data[, -1], 2, sd, na.rm = TRUE)

# Print additional statistics
cat("Means:\n")
print(means)
cat("Medians:\n")
print(medians)
cat("Standard Deviations:\n")
print(sds)

