#!/usr/bin/env Rscript

# pnorm gives area
# qnorm gives z value

# T table
#for (p in c(0.5, 0.25, 0.2, 0.15, 0.1, 0.05, 0.025, 0.01)) {
#	for (f in 1:30) {
#		print(qt(p, df=f))
#	}
#}

avocado_data <- read.csv("avocado.csv")

boston <- avocado_data[avocado_data[,"region"] == "Boston",]
boston_total <- boston[,"Total.Volume"]

atlanta <- avocado_data[avocado_data[,"region"] == "Atlanta",]
atlanta_total <- atlanta[,"Total.Volume"]

confidence = 0.95
z_value <- qnorm(confidence)

n_one <- length(boston_total)
n_two <- length(atlanta_total)

mean_one <- mean(boston_total)
mean_two <- mean(atlanta_total)

var_one <- var(boston_total)
var_two <- var(atlanta_total)

v <- ((var_one / n_one + var_two / n_two)^2) / (((var_one / n_one)^2) / (n_one - 1) + ((var_two / n_two)^2) / (n_two - 1))

t <- - qt((1 - confidence) / 2 , df=v)

std_err <- sqrt((var_one / n_one) + (var_two / n_two))

estimate <- mean_one - mean_two

print("Confidence interval:")
print(sprintf("%s - %s * %s < mu_one - mu_two < %s + %s * %s", estimate, t, std_err, estimate, t, std_err))
print(sprintf("%s - %s < mu_one - mu_two < %s + %s", estimate, t * std_err, estimate, t * std_err))

#error_range <- z_value * sqrt(() + ())
#error_range <- z_a/2 sqrt(p1 q1 / n1 + p2 q2 / n2)
