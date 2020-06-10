#!/usr/bin/env Rscript

library(moments)

printf <- function(...) {
	cat(sprintf(...), "\n")
}

fractile <- function(data_set, n) {
	name <- "Fractile"
	if (n ==   3) name <- "Tertile"
	if (n ==   4) name <- "Quartile"
	if (n ==   5) name <- "Quintile"
	if (n ==   6) name <- "Sextile"
	if (n ==   7) name <- "Septile"
	if (n ==   8) name <- "Octile"
	if (n ==  10) name <- "Decile"
	if (n ==  12) name <- "Duo-decile"
	if (n ==  16) name <- "Hexadecile"
	if (n ==  20) name <- "Ventile"
	if (n == 100) name <- "Percentile"
	for (i in 2:n-1) {
		printf("%s %s = %s", name, i, quantile(data_set, i * (1/n)))
	}
}

basic_descriptive <- function(data_set) {
	printf("Variance = %s", var(data_set))
	printf("Standard Deviation = %s", sd(data_set))
	range <- range(data_set)
	printf("Range = %s - %s", range[1], range[2])
	fractile(data_set, 4)
	fractile(data_set, 100)
	printf("Skewness = %s", skewness(data_set))
}

avocado_data <- read.csv("avocado.csv")

atlanta <- avocado_data[avocado_data[,"region"] == "Atlanta",]
atlanta_total <- atlanta[,"Total.Volume"]

printf("The data set describes avacodo sales with prices and the amount of units in certain regions of America")
printf("Accessed from : https://www.kaggle.com/neuromusic/avocado-prices")
printf("Variables :")
printf("Date         - Interval - The date the entry was recorded at")
printf("AveragePrice - Interval - The average price of an avacado in the entry")
printf("Total Volume - Interval - The total count of avocado sold in the entry")
printf("4046         - Interval - The count of avocados sold with code 4046 in the entry")
printf("4225         - Interval - The count of avocados sold with code 4225 in the entry")
printf("4770         - Interval - The count of avocados sold with code 4770 in the entry")
printf("Total Bags   - Interval - The total bags of avocado sold in the entry")
printf("Small Bags   - Interval - The count of small bags sold in the entry")
printf("Large Bags   - Interval - The count of large bags sold in the entry")
printf("XLarge Bags  - Interval - The count of extra large bags sold in the entry")
printf("type         - Nominal  - Whether the sold avocados are organic or conventional")
printf("year         - Interval - The year the entry was recorded at")
printf("region       - Nominal  - The region where the data entry was located at")

basic_descriptive(atlanta_total)
plot(atlanta_total, atlanta[,"AveragePrice"])
boxplot(atlanta_total)
