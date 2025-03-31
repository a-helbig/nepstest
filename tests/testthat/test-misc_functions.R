test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

expect_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, NA), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

expect_data2 <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

expand_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), type=c(1,2,1,1), dur=c(10,12,1,5))

# var_exists
test_that("var_exists: test main feature of function", {
  expect_error(var_exists(test_data, "kieubeegs")) # test if it throws error when a non existing variable is passed
  expect_no_error(var_exists(test_data, "kieubeeg")) # test if there no error when an existing variable is passed
  expect_no_error(var_exists(test_data, c("kieubeeg","modak"))) # test if it accepts a vector of variables
  expect_output(var_exists(test_data, "kieubeeg")) # test if theres a print statement if test passes
})

test_that("var_exists: test arguments", {
  expect_error(var_exists(test_dataz, "kieubeeg")) # test if data argument errors when dataset is non-existent
  expect_error(var_exists(test_data, kieubeegs)) # test if variable argument without quote errors
})

# date_test
test_that("date_test: test main feature of function and stops", {
  expect_no_error(date_test(test_data, "ezstm", "ezstj", "larger","intmPRE","intjPRE")) # test if it throws no error when start date is actually always larger than intdatePRE
  expect_no_error(date_test(replace_season_codes(test_data), "ezstm", "ezstj", "smaller","intm","intj")) # test if it throws no error when end date is actually always smaller than intdate
  expect_error(date_test(test_data, "ezendm", "ezendj", "smaller","intm","intj")) # test if it throws error when enddate is actually larger than intdate
  expect_error(date_test(test_data, "ezendm", "ezendj", "smallerr","intm","intj")) # test if it throws error when invalid operator argument is used
  expect_error(date_test(test_data, "ezendM", "ezendj", "smaller","intm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezentj", "smaller","intm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezend", "smaller","inttm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezend", "smaller","intm","intjj")) # test if first stop works
})

# replace_values_with_na
test_that("replace_values_with_na: test main feature of function", {
  expect_equal(replace_values_with_na(test_data), expect_data) # test if function works as intended with data arg only
  expect_equal(replace_values_with_na(test_data, vars = "ezendj"), expect_data) # test if function works as intended with data and correct var arg
  expect_false(isTRUE(all.equal(replace_values_with_na(test_data, vars = "ezendm"), expect_data))) # test if function works as intended with data and var arg where var argument is the wrong one without any values that should be repalced with NA
})

# replace_season_codes
test_that("replace_season_codes: test main feature of function", {
  expect_equal(replace_season_codes(test_data), expect_data2) # test if function works as intended with data arg only
  expect_equal(replace_season_codes(test_data, vars = "ezstm"), expect_data2) # test if function works as intended with data and correct var arg
  expect_false(isTRUE(all.equal(replace_season_codes(test_data, vars = "ezendm"), expect_data2))) # test if function works as intended with data and var arg where var argument is the wrong one without any values that should be repalced with NA
})

# expand
test_that("expand: test main feature of function", {
  # expect_equal(expand(expand_data), expect_data2) # test if function works as intended with data arg only
  # expect_equal(expand(expand_data, vars = "ezstm"), expect_data2) # test if function works as intended with data and correct var arg

})

# test <- expand(expand_data, "dur")
