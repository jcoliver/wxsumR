# Compare rainfall output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

rm(list = ls())

################################################################################
library(haven) # To read in STATA .dta files
library(tidyverse)
r_results_file <- "data/essy2_rf_weathercommand.csv"
stata_results_file <- "../large-data-files/essy2_x3_rf2_daily_out.dta"

r_results <- read_csv(r_results_file)
stata_results <- read_dta(stata_results_file)
stata_results <- stata_results[stata_results$household_id2 != 0, ]

r_results <- r_results[order(r_results$household_id2), ]
stata_results <- stata_results[order(stata_results$household_id2), ]

r_results$household_id2 <- as.factor(r_results$household_id2)
stata_results$household_id2 <- as.factor(stata_results$household_id2)

# Differences in size of objects partly due to difference in dry calculations;
# weathercommand in R includes start and end dry days, stata does not
# But there are additional differences the same site appears represented
# in multiple rows of stata output, with different values; look at
# household_id2 = 150102088803102080
# And, the R results look like the id is being rounded at some point, likely
# right when the original data are being read in. Probably best to update the
# R functions so the id column is treated as a factor?

r_results[1:6, 1:5]
stata_results[1:6, 1:5]

length(intersect(x = r_results$household_id2, y = stata_results$household_id2))
