# PART 5 - Two Sample Hypothesis Testing for Total Sales in Boston and Atlanta Regions
# We will apply t-test. Assume that each groups are APPROXIMATELY NORMALLY DISTRIBUTED WITH EQUAL VARIANCES.

library(data.table)

# read the data as data table
avo <- as.data.table(read.csv("avocado.csv"))

# 1. State the hypothesis.
# H0: there is NO significant difference between total sales (in volume) for Boston and Atlanta regions.
# H1: there is a significant difference between total sales (in volume) for Boston and Atlanta regions.
# Notice that this test has a two-tailed observation.

atlanta <- avo[avo$region == "Atlanta", ]$Total.Volume
boston <- avo[avo$region == "Boston", ]$Total.Volume
# 2. Compute the test statistic.

t.test(atlanta, boston, paired = F, alternative = "two.sided", var.equal = T, conf.level = 0.95)
t_value <- (mean(atlanta)-mean(boston))/sqrt(var(atlanta)/338 + var(boston)/338)
print(t_value)
# 3. Interpret the results.
# t_value = -1.217 with df = 674 and p = 0.224. p > 0.025 (two-tailed test, conf. level = 0.95 then alpha = 0.05)
# reject null hypothesis in favor of alternative hypothesis.
# there is a significant difference between means for Boston and Atlanta regions.

## PART6 - Regression Analysis: "4046 and "Total Bags" in Boston region

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
