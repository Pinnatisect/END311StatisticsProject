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
