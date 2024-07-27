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


# Perform linear regression analysis
# Assume Sales is the dependent variable and others are independent variables
model <- lm(Sales ~ ., data = data[, -1])

# Print summary of the model to get t-values, p-values, and F-statistic
model_summary <- summary(model)
print(model_summary)

# Extract coefficients, t-values, and p-values for coefficients
coefficients <- model_summary$coefficients
t_values <- coefficients[, "t value"]
p_values_coefficients <- coefficients[, "Pr(>|t|)"]

# Print coefficients, t-values, and p-values for coefficients
cat("Coefficients:\n")
print(coefficients)
cat("T-values:\n")
print(t_values)
cat("P-values for coefficients:\n")
print(p_values_coefficients)

# Extract F-statistic and its p-value
f_statistic <- model_summary$fstatistic
f_value <- f_statistic["value"]
df1 <- f_statistic["numdf"]
df2 <- f_statistic["dendf"]
p_value_f <- pf(f_value, df1, df2, lower.tail = FALSE)

# Print F-statistic and its p-value
cat("F-statistic:\n")
print(f_value)
cat("Degrees of freedom 1 (numerator):\n")
print(df1)
cat("Degrees of freedom 2 (denominator):\n")
print(df2)
cat("P-value for F-statistic:\n")
print(p_value_f)

