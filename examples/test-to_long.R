# Test of to_long function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

rm(list = ls())
################################################################################
library(wxsumR)

infile <- "data/rain-medium.Rds"
df <- readRDS(file = infile)
long_df <- wxsumR:::to_long(data = df)

if (round(mean(long_df$value, na.rm = TRUE), digits = 4) == 2.8695) {
  message("Test 1 of to_long PASS")
} else {
  message("Test 1 of to_long FAIL")
}
