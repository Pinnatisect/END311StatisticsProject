#!/usr/bin/env Rscript

printf <- function(...) {
	cat(sprintf(...), "\n")
}

avocado_data <- read.csv("avocado.csv")

boston <- avocado_data[avocado_data[,"region"] == "Boston",]
boston_total <- boston[,"Total.Volume"]

atlanta <- avocado_data[avocado_data[,"region"] == "Atlanta",]
atlanta_total <- atlanta[,"Total.Volume"]

confidence = 0.95

n_one <- length(boston_total)
n_two <- length(atlanta_total)

mean_one <- mean(boston_total)
mean_two <- mean(atlanta_total)

var_one <- var(boston_total)
var_two <- var(atlanta_total)

v <- ((var_one / n_one + var_two / n_two)^2) / (((var_one / n_one)^2) / (n_one - 1) + ((var_two / n_two)^2) / (n_two - 1))

t <- (-qt((1 - confidence)/2, df=v))

std_err <- sqrt((var_one / n_one) + (var_two / n_two))

estimate <- mean_one - mean_two

printf("mean_one=%s", mean_one)
printf("mean_two=%s", mean_two)
printf("estimate=%s", estimate)
printf("t=%s", t)
printf("v=%s", v)
printf("Confidence interval:")
printf(sprintf("%.2f - %.2f * %.2f < mu_one - mu_two < %.2f + %.2f * %.2f", estimate, t, std_err, estimate, t, std_err))
printf(sprintf("%.4f - %.4f < mu_one - mu_two < %.4f + %.4f", estimate, t * std_err, estimate, t * std_err))
printf(sprintf("%s < mu_one - mu_two < %s", estimate - t * std_err, estimate + t * std_err))
