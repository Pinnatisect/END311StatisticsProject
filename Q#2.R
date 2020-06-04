# PART2 : Hypothesis testing for mean 
library(data.table)

my_avocado_data <- read.csv("avocado.csv", sep = ",", header = T)
atlanta_data <- subset(my_avocado_data, region == "Atlanta")

# 1. State the hypothesis.
# H0(Null hypothesis): mean of total amount of avocado sold (in volume) in Atlanta region is equal to 200000(test value)
# H1(alternative hypothesis): mean of total amount of avocado sold (in volume) in Atlanta region is greater than 200000(test value)

atlanta <- atlanta_data$Total.Volume
test_mu <- 200000

# 2. Compute the test statistic.
t.test(atlanta, mu = 200000, alternative = "greater", var.equal = T, conf.level = 0.95)
t_value <- (mean(atlanta) - test_mu)/ sqrt(var(atlanta)/nrow(atlanta_data))

# 3. Interpret the results.
# t_value = 4.3424 with df = 337 and p-value = 2e+05 which means 200000 (conf. level = 0.95 then alpha = 0.05)
# reject null hypothesis in favor of alternative hypothesis.
# mean of total amount of avocado sold in Atlanta region is greater than 200000
