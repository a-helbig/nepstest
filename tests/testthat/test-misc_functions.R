test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,12,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2021,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors2 <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2021,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors3 <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2022,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

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
  expect_no_error(date_test(test_data, "ezstm", "ezstj", "smaller","intm","intj")) # test if it throws no error when end date is actually always smaller than intdate
  expect_error(date_test(test_data, "ezendm", "ezendj", "smaller","intm","intj")) # test if it throws error when enddate is actually larger than intdate
  expect_error(date_test(test_data, "ezendm", "ezendj", "smallerr","intm","intj")) # test if it throws error when invalid operator argument is used
  expect_error(date_test(test_data, "ezendM", "ezendj", "smaller","intm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezentj", "smaller","intm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezend", "smaller","inttm","intj")) # test if first stop works
  expect_error(date_test(test_data, "ezendm", "ezend", "smaller","intm","intjj")) # test if first stop works
})

# date_test_complex
test_that("date_test_complex: test main feature of function and stops", {
  expect_no_error(date_test_complex(test_data, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws no error when there are none
  expect_no_error(date_test_complex(test_data, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj")) # test if it throws no error when there are none and only a single event is tested
  expect_error(date_test_complex(test_data_with_errors, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when enddate is actually larger than intdate
  expect_error(date_test_complex(test_data_with_errors2, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when startyear is actually smaller than intjPRE
  expect_error(date_test_complex(test_data_with_errors3, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when var2 is smaller than var1
})

replace_na_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(-97,8,12,11), ezendj = c(-97,-98,2024, NA), eziz = c(2,2,2,2), ezmod = c(-97,2,2,2), kieubeeg = c(-56,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_na_data_replaced <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(NA,8,12,11), ezendj = c(NA,NA,2024, NA), eziz = c(2,2,2,2), ezmod = c(NA,2,2,2), kieubeeg = c(NA,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_na_data_replaced_error <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(-97,8,12,11), ezendj = c(2024,2024,2024, NA), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

# replace_values_with_na
test_that("replace_values_with_na: test main feature of function", {
  expect_equal(replace_values_with_na(replace_na_data), replace_na_data_replaced) # test if function works as intended with data arg only
  expect_equal(replace_values_with_na(replace_na_data, vars = c("ezendm", "ezendj","ezmod","kieubeeg")), replace_na_data_replaced) # test if function works as intended with data and correct var arg
  expect_false(isTRUE(all.equal(replace_values_with_na(replace_na_data, vars = "ezendm"), replace_na_data_replaced_error))) # test if function works as intended when there is no replacement of -97 in ezendm
})

replace_season_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,27,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,24,32,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(21,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_season_data_replaced <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,7,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,4,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(1,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_season_data_error <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,7,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,4,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(21,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

# replace_season_codes
test_that("replace_season_codes: test main feature of function", {
  expect_equal(replace_season_codes(replace_season_data, vars = c("ezendm", "intm","ezstm")), replace_season_data_replaced) # test if function works as intended with data arg only
  expect_error(replace_season_codes(replace_season_data)) # test if function errors when vars arg is missing
})

data_to_expand <- data.frame(caseid = c(12323154,21411451), dauer = c(2,3))
expanded_data <- data.frame(caseid = c(12323154,12323154,21411451,21411451,21411451), dauer = c(2,2,3,3,3))

# expand
test_that("expand: test main feature of function", {
  expect_equal(expand(data_to_expand, "dauer"), expanded_data) # test if function works as intended with data arg only
  expect_error(expand(expand_data)) # test if function errors when duration argument is missing
  expect_error(expand(list(expand_data))) # test if function errors when data argument is not a df
  expect_error(expand(expand_data, "daur")) # test if function errors when dur argument is not found in data
  expect_error(expand(data.frame(caseid = c(12323154), dauer = c(-2)), "dauer")) # test if function errors when duration is negative numeric
  expect_error(expand(data.frame(caseid = c(12323154), dauer = c("-2")), "dauer")) # test if function errors when duration is a string
  expect_error(expand(data.frame(caseid = c(12323154,12323153), dauer = c(2, NA)), "dauer")) # test if function errors when duration is a string
})


