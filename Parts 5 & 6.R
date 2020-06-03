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

# 3. Interpret the results.
# t_value = -1.217 with df = 674 and p = 0.224. p > 0.025 (two-tailed test, conf. level = 0.95 then alpha = 0.05)
# reject null hypothesis in favor of alternative hypothesis.
# there is a significant difference between means for Boston and Atlanta regions.

## PART6 - Regression Analysis: "4047 and "Total Bags" in Boston region

# 1. Obtain the equation of the regression line.

# We should first subset Atlanta region from our dataset.

at <- avo[region == "Atlanta"]
model <- lm(Total.Bags ~ X4225, data = at )
# our formula should be stated as >> Total Bags = 19306.845 + 2.088  (Avocados with code 4225)

# 2. Graph the line on a scatter diagram.
# We should first call ggplot2 library.
?stat_summary
library(ggplot2)
?ggplot2
ggplot(at, aes(X4225, Total.Bags), ) + 
  geom_point() + stat_smooth(method = lm )

# 3. Interpret R-squared value
summary(model)
# R-squared takes a value between 0 and 1. Here the summary gives us two different values for R-squared: multiple and adjusted.
# Multiple R-squared:  0.5779,	Adjusted R-squared:  0.5767 
# We are interested in adjusted R-squared value. Multiple R-squared includes a penalty for multiple linear regression.
# 0.5767 means that our model is able to explain the variability. However, a value closer to 1 would be preferred. 
# We can observe that there's a cumulation for small values.

# 4. Hypothesis testing for the slope and intercept.

# H0: The coefficient is equal to zero (no correlation between Total Bags and Avocado type 4225.)
# H1: The coefficient is NOT equal to zero. (There is a correlation between Total Bags and Avocado type 4225.)
summary(model)
# Again, using the summary we can say that....
