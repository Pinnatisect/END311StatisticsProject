## PART6 - Regression Analysis: "4046 and "Total Bags" in Boston region

library(data.table)

# read the data as data table
avo <- as.data.table(read.csv("avocado.csv"))

# 1. Obtain the equation of the regression line.

# We should first subset Atlanta region from our dataset.

options(scipen = 999) # disabling scientific notation to work easier with numbers.
at <- avo[region == "Atlanta"]
model <- lm(Total.Bags ~ X4046, data = at )
print(model)
# our formula should be stated as >> Total Bags = 20554.7389 + 0.4376  (Avocados with code 4046)

# 2. Graph the line on a scatter diagram.
# We should first call ggplot2 library.
library(ggplot2)
ggplot(at, aes(X4046, Total.Bags)) + ggtitle("Type 4046 vs. Total Bags") + xlab("Avocado Type 4046")+ ylab ("Total Bags") + 
  geom_point() + geom_smooth(method = lm, se= FALSE) +   theme(plot.title = element_text(hjust = 0.5))

# 3. Interpret R-squared value
summary(model)

# R-squared value is used to interpret how well our model fits the data.
# R-squared takes a value between 0 and 1. Here the summary gives us two different values for R-squared: multiple and adjusted.
# Multiple R-squared:  0.4503,	Adjusted R-squared:  0.4487 
# We are interested in adjusted R-squared value. Multiple R-squared includes a penalty for multiple linear regression models.
# 0.4487 value means that our model is not able to explain the variability (only partially). A value closer to 1 would be preferred. 
# We can observe that from Residual standard error value as well: 74,800. RSE value is relatively high, which means that the linear model fits our data poorly. The values deviate from our regression line by 74,800 units.
# Also the percentage error for RSE is calculated by sigma(model) * 100 / mean(at$Total.Bags) = 88.52812. 

# 4. Hypothesis testing for the slope and intercept.
# 4.1: Hypothesis testing on the slope
# H0: The coefficient is equal to zero (no correlation between Total Bags and Avocado type 4046.)
# H1: The coefficient is NOT equal to zero. (There is a correlation between Total Bags and Avocado type 4046.)

b1 <- model$coefficients[2]
n <- nrow(at)
SSE <- sum((at$Total.Bags - mean(at$Total.Bags))^2) - b1*(sum((at$X4046 - mean(at$X4046))* (at$Total.Bags - mean(at$Total.Bags))))
s <- sqrt(SSE / (n-2))
sxx <- sum((at$X4046 - mean(at$X4046))^2)
# t-value we get from the test:
t_slope <- (b1) / (s/ sqrt(sxx))
# our critical t value is: (%95 confidence interval)
t_critical <- qt(0.975, df = 336)
# t_slope > t_slope_critical, falls into critical region. Therefore we reject H0, the b1 coefficient cannot be equal to 0. (There's a correlation.)

# 4.2. Hypothesis testing on the intercept
# H0: The b0 coefficient (intercept) is equal to 0.
# H1: The b0 coefficient (intercept) is different than 0.

b0 <- model$coefficients[1]
denom <- sqrt(sum((at$X4046)^2/ (n*sxx)))
t_intercept <- (b0) / (s*denom)
# t_intercept = 3.667606 > t_critical = 1.967, falls into critical region. Therefore we reject H0, the intercept cannot be equal to 0.

# We can also check the t-values from summary(model), under the "Coefficients" table, which also confirms our results.

# ADDITIONAL - REMOVE
#regression <- summary(model) 
# coefficients <- regression$coefficients
# beta_estimate <-coefficients["X4046", "Estimate"]
#stderror <- coefficients["X4046", "Std. Error"]
# regression_t <- beta_estimate / stderror
# regression_p <- 2*pt(-abs(regression_t), df=nrow(at)-ncol(at))
# print(regression_t)
# Again, using t-value for our hypothesis is 16.59131. 
# It simply tells us that the coefficient cannot be equal to zero. 
# Checking our summary model, we can see that P-value is extremely small and falls into critical region ( <alpha = 0.05) We can reject H0.
