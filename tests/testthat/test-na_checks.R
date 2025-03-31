test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,2,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, 2024), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_that("test function no_na_in_vars", {
  expect_error(no_na_in_vars(test_data, "kieubeeg")) # core feature: test if it errors when variable is passed with NA
  expect_output(no_na_in_vars(test_data, "ezmod")) # core feature: test if it errors when variable is passed with NA
  expect_error(no_na_in_vars(test_data, data.frame(c("ezmod")))) # test if fct errors when we pass a list as 2. argument
  expect_error(no_na_in_vars(test_data, c("ezmod","esnr"))) # test if fct errors when we pass a vector with varname thats not in data
})


test_that("test function only_na_in_vars", {
  expect_no_error(only_na_in_vars(test_data, "kieubeeg", "intm", "equal", "12")) # test if it does not error when it shouldnt
  expect_error(only_na_in_vars(test_data, "kieubeeg", "intm", "equals", "11")) # core feature: test if it errors when it should
})


