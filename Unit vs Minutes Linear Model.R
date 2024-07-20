# Set path
setwd("E:/Academic S7/MA4014 - Linear Models and Multivariate Statistics/Repository/Linear-Models")
current_path <- getwd()

# Read data
data <- read.table("comprep.txt", header = TRUE)

# Correlation
correlation <- cor(data$Minutes, data$Units)
print(paste('Correlation: ', correlation))

# Regression model: Units as a function of Minutes
model <- lm(Minutes ~ Units, data = data)

# Plotting Units on x-axis and Minutes on y-axis
plot(data$Units, data$Minutes,
     main = "Units vs. Minutes",
     xlab = "Units",
     ylab = "Minutes",
     pch = 20,      
     col = "blue")   

summary(model)

# Add the regression line to the plot
abline(lm(Minutes ~ Units, data = data), col = "red", lwd = 2)



