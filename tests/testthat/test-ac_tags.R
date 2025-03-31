test_that("ac_test throws an error when targetvar is not equal to target value", {
  test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,2,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, 2024), eziz = c(2,2,2,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

  expect_no_error(ac_test(test_data, "ezmod", 2, "modak", "equal", 29))  # test if valid values throws no error
  expect_error(ac_test(test_data, "ezmod", 3, "kieubeeg", "equal", 2)) # test if non valid values throws error
  expect_no_error(ac_test(test_data, "ezmod", "kieubeeg", "caseid", "inlist", c("12393535","87122445"))) # test if a variable as target value works too as intended
  expect_error(ac_test(test_data, "ezmot", "kieubeeg", "caseid", "inlist", c("12393535","87122445"))) # test if first stop works
  expect_error(ac_test(test_data, "ezmod", "kieubeeg", "caseed", "inlist", c("12393535","87122445"))) # test if second stop works
})
