test_that("ac_test throws an error when targetvar is not equal to target value", {
  test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,2,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, 2024), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

  expect_no_error(ac_test(test_data, "ezmod", 2, "modak", "equal", 29))  # test if valid values throw no error
  expect_error(ac_test(test_data, "ezmod", 3, "kieubeeg", "equal", 2)) # test if non valid values throw error
  expect_no_error(ac_test(test_data, "ezmod", "kieubeeg", "caseid", "inlist", c("12393535","87122445"))) # test if a variable as target value works too as intended
  expect_error(ac_test(test_data, "ezmot", "kieubeeg", "caseid", "inlist", c("12393535","87122445"))) # test if first stop works
  expect_error(ac_test(test_data, "ezmod", "kieubeeg", "caseed", "inlist", c("12393535","87122445"))) # test if second stop works
})



# Mock subset_data function to allow testing the condition subsetting behavior
subset_data <- function(data, condition_var, condition_value, operator) {
  # Minimal implementation for testing purposes
  if (operator == "equal") {
    data[data[[condition_var]] == as.numeric(condition_value), ]
  } else {
    data
  }
}

# Example test data
test_data <- data.frame(
  caseid = 1:5,
  var1 = c(1, 2, 2, NA, 1),
  var2 = c(1, 2, 3, NA, 1),
  var3 = c("A", "B", "B", NA, "A"),
  stringsAsFactors = FALSE
)

test_that("ac_test succeeds when numeric target_value matches target_var", {
  expect_message(ac_test(test_data, "var1", c(1, 2)),
                 regexp = "SUCCESS: The 'var1' variable contains only values")
})

test_that("ac_test fails when numeric target_value not matching target_var", {
  expect_error(ac_test(test_data, "var1", c(1)),
               regexp = "Problematic caseid\\(s\\): 2, 3")
})

test_that("ac_test succeeds when character target_value variable matches target_var", {
  # create data where var1 and var3 match (NA considered equal)
  df <- data.frame(caseid = 1:4,
                   var1 = c("A", "B", NA, "C"),
                   var2 = c("A", "B", NA, "C"),
                   stringsAsFactors = FALSE)
  expect_message(ac_test(df, "var1", "var2"),
                 regexp = "SUCCESS: The 'var1' variable equals variable 'var2'")
})

test_that("ac_test fails when character target_value variable not matching target_var", {
  df <- data.frame(caseid = 1:3,
                   var1 = c("A", "B", "C"),
                   var2 = c("A", "X", "C"),
                   stringsAsFactors = FALSE)
  expect_error(ac_test(df, "var1", "var2"),
               regexp = "Problematic caseid\\(s\\): 2")
})

test_that("ac_test errors when target_var variable not in dataframe", {
  expect_error(ac_test(test_data, "not_exist", c(1)),
               regexp = "Variable not_exist not found")
})

test_that("ac_test errors when caseid variable not in dataframe", {
  df <- test_data
  df$caseid <- NULL  # Remove caseid
  expect_error(ac_test(df, "var1", c(1)),
               regexp = "Case ID variable caseid not found")
})

test_that("ac_test errors when condition_var is specified but missing", {
  expect_error(ac_test(test_data, "var1", c(1), condition_var = "not_exist", condition_value = "1"),
               regexp = "Condition variable not_exist not found")
})

test_that("ac_test subsets data with condition_var and condition_value", {
  # Using our minimal subset_data that filters var1 == condition_value
  df <- data.frame(caseid = 1:5, var1 = c(1,2,1,1,3), var2 = c(1,1,1,1,1))
  expect_message(ac_test(df, "var1", c(1), condition_var = "var1", condition_value = "1"),
                 regexp = "SUCCESS")
  # Only rows with var1 == 1 remain, so test passes
})

test_that("ac_test errors when character target_value var not found in data", {
  expect_error(ac_test(test_data, "var1", "not_exist_var"),
               regexp = "Target value'")
})

test_that("ac_test errors on invalid target_value type", {
  expect_error(ac_test(test_data, "var1", list(1,2)),
               regexp = "target_value must be either numeric or a character variable name")
})




# new tests:
library(testthat)

# Assuming your ac_test function is already available in the environment.

test_that("error if target_var not in data (no condition params)", {
  df <- data.frame(caseid = 1:3, A = 1:3)
  expect_error(ac_test(df, "Z", 1), "Variable Z not found")
})

test_that("error if caseid column is missing (no condition params)", {
  df <- data.frame(id = 1:3, A = 1:3)
  expect_error(ac_test(df, "A", 1), "Case ID variable caseid not found")
})

test_that("numeric target_value success when all match (no condition params)", {
  df <- data.frame(caseid = 1:3, A = c(5,5,5))
  expect_message(ac_test(df, "A", 5), "SUCCESS")
})

test_that("numeric target_value failure when some values differ (no condition params)", {
  df <- data.frame(caseid = 1:4, A = c(5,5,6,5))
  expect_error(ac_test(df, "A", 5), "Problematic caseid")
})

test_that("character target_value as variable name success when all equal (no condition params)", {
  df <- data.frame(caseid = 1:3, A = c("foo", "bar", NA), B = c("foo", "bar", NA), stringsAsFactors = FALSE)
  expect_message(ac_test(df, "A", "B"), "SUCCESS")
})

test_that("character target_value as variable name failure when values differ (no condition params)", {
  df <- data.frame(caseid = 1:3, A = c("foo", "bar", "baz"), B = c("foo", "bar", NA), stringsAsFactors = FALSE)
  expect_error(ac_test(df, "A", "B"))
})

test_that("error if target_value character var name not found in data (no condition params)", {
  df <- data.frame(caseid = 1:2, A = c(1,2))
  expect_error(ac_test(df, "A", "nonexistent_var"))
})

test_that("error if target_value is neither numeric nor character (no condition params)", {
  df <- data.frame(caseid = 1:2, A = c(1,2))
  expect_error(ac_test(df, "A", list(1,2)), "target_value must be either numeric or a character variable name in data")
})

test_that("NA values are ignored properly (numeric target_value, no condition params)", {
  df <- data.frame(caseid = 1:4, A = c(NA, 7, 7, NA))
  expect_message(ac_test(df, "A", 7), "SUCCESS")

  df2 <- data.frame(caseid = 1:4, A = c(NA, 7, 8, NA))
  expect_error(ac_test(df2, "A", 7), "Problematic caseid")
})

test_that("NA values are ignored properly (character target_value, no condition params)", {
  df <- data.frame(caseid = 1:3, A = c(NA, "dog", "cat"), B = c(NA, "dog", "cat"), stringsAsFactors = FALSE)
  expect_message(ac_test(df, "A", "B"), "SUCCESS")

  df2 <- data.frame(caseid = 1:3, A = c(NA, "dog", "fish"), B = c(NA, "dog", "cat"), stringsAsFactors = FALSE)
  expect_error(ac_test(df2, "A", "B"), "Problematic caseid")
})

