test_data <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,12,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2021,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors2 <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2021,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_data_with_errors3 <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,1,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2022,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

# date_test_complex
test_that("date_test_complex: test main feature of function and stops", {
  expect_no_error(date_test_complex(test_data, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws no error when there are none
  expect_no_error(date_test_complex(test_data, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj")) # test if it throws no error when there are none and only a single event is tested
  expect_error(date_test_complex(test_data_with_errors, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when enddate is actually larger than intdate
  expect_error(date_test_complex(test_data_with_errors2, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when startyear is actually smaller than intjPRE
  expect_error(date_test_complex(test_data_with_errors3, "intmPRE","intjPRE","intm", "intj", "ezstm", "ezstj", "ezendm", "ezendj")) # test if it throws error when var2 is smaller than var1
})

replace_na_data <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(-97,8,12,11), ezendj = c(-97,-98,2024, NA), eziz = c(2,2,2,2), ezmod = c(-97,2,2,2), kieubeeg = c(-56,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_na_data_replaced <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(NA,8,12,11), ezendj = c(NA,NA,2024, NA), eziz = c(2,2,2,2), ezmod = c(NA,2,2,2), kieubeeg = c(NA,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_na_data_replaced_error <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,21,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(-97,8,12,11), ezendj = c(2024,2024,2024, NA), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

# replace_values_with_na
test_that("replace_values_with_na: test main feature of function", {
  expect_equal(replace_values_with_na(replace_na_data), replace_na_data_replaced) # test if function works as intended with data arg only
  expect_equal(replace_values_with_na(replace_na_data, vars = c("ezendm", "ezendj","ezmod","kieubeeg")), replace_na_data_replaced) # test if function works as intended with data and correct var arg
  expect_false(isTRUE(all.equal(replace_values_with_na(replace_na_data, vars = "ezendm"), replace_na_data_replaced_error))) # test if function works as intended when there is no replacement of -97 in ezendm
})

replace_season_data <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,27,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,24,32,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(21,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_season_data_replaced <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,7,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,4,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(1,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

replace_season_data_error <- data.frame(caseid = c(99999991, 99999992, 99999993,99999994), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,7,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,4,12,11), ezendj = c(2024,2024,2024, -98), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(21,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

# replace_season_codes
test_that("replace_season_codes: test main feature of function", {
  expect_equal(replace_season_codes(replace_season_data, vars = c("ezendm", "intm","ezstm")), replace_season_data_replaced) # test if function works as intended with data arg only
  expect_error(replace_season_codes(replace_season_data)) # test if function errors when vars arg is missing
})
