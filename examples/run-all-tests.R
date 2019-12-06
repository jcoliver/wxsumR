# Run all tests
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-12-06

rm(list = ls())

################################################################################
test_files <- list.files(path = "examples", pattern = "test-", full.names = TRUE)

message("Running all tests in examples folder.")
for (one_file in test_files) {
  source(file = one_file)
}
message("****   All Tests Complete    ****")