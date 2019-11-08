# Test of toLong function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

################################################################################
source(file = "R/to_long.R")

library(tidyverse)
df <- read.csv(file = "data/input-tiny.csv")
long_df <- to_long(data = df)

df <- read.csv(file = "data/input-small.csv")
long_df <- to_long(data = df)

df <- read.csv(file = "data/input-messy.csv")
long_df <- to_long(data = df) # Should throw warning
