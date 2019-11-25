# Test of to_long function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

################################################################################
source(file = "R/to_long.R")

library(tidyverse)
df <- read.csv(file = "data/input-rain-tiny.csv")
long_df <- to_long(data = df)

df <- read.csv(file = "data/input-rain-small.csv")
long_df <- to_long(data = df)

df <- read.csv(file = "data/input-rain-messy.csv")
long_df <- to_long(data = df) # Should throw warning
