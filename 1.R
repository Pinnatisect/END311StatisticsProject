# First, read the avocado file with read.csv. 
# Notice that the dataframe contains headers, so we set header argument as True.
library(data.table)
my_avocado_data <- read.csv("avocado.csv", sep = ",", header = T)
# What is the class of our data? We get a data.frame class.
class(my_avocado_data)
# The attributes of our data are column names, row names and the class of data.
attributes(my_avocado_data)
# We can extract colunm names only with colnames.
colnames(my_avocado_data)
# We are investigating Atlanta data so we should 
atlanta_data <- subset(my_avocado_data, region == "Atlanta")
atlanta_data
