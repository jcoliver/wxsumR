# Test of toLong function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

################################################################################
source(file = "R/to_long.R")

library(tidyverse)
df <- read.csv(file = "data/tiny-input.csv")
long_df <- toLong(data = df)
