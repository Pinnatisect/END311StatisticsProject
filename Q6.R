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
  geom_point() + stat_smooth(method = lm)

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

# H0: The coefficient is equal to zero (no correlation between Total Bags and Avocado type 4225.)
# H1: The coefficient is NOT equal to zero. (There is a correlation between Total Bags and Avocado type 4225.)
regression <- summary(model) 
coefficients <- regression$coefficients
beta_estimate <-coefficients["X4046", "Estimate"]
stderror <- coefficients["X4046", "Std. Error"]
regression_t <- beta_estimate / stderror
regression_p <- 2*pt(-abs(regression_t), df=nrow(at)-ncol(at))
print(regression_t)
# Again, using t-value for our hypothesis is 16.59131. 
# It simply tells us that the coefficient cannot be equal to zero. 
# Checking our summary model, we can see that P-value is extremely small and falls into critical region ( <alpha = 0.05) We can reject H0.
