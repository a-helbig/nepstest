test_data <- data.frame(caseid = c(12393535, 12305623, 87122445,91235212), zs1 = c("2024/12/02 1520:55","2025/10/13 1341:47","2025/12/22 1251:51","2024/11/03 1533:53"), modak = c(29,29,29,29), eznr = c(1,1,1,1), ezstm = c(1,5,2,NA), ezstj = c(2024,2024,2024,2024), ezendm = c(6,8,12,11), ezendj = c(2024,2024,2024, NA), eziz = c(NA,NA,NA,2), ezmod = c(2,2,2,2), kieubeeg = c(2,1,2,NA), intm = c(10,11,10,12), intj = c(2024,2024,2024,2024), intmPRE = c(12,12,11,9), intjPRE = c(2023,2023,2022,2023))

test_that("af_test_simple: test main feature of function and stops", {
  expect_no_error(af_test_simple(test_data, "intm", "intj")) # test if it throws no error when we pass valid  data, var1, var2 arguments
  expect_error(af_test_simple(test_data, "ezmod", "kieubeeg")) # test if it throws error when there is no NA equivalence between var1 and var2
  expect_error(af_test_simple(test_data, "intm", "intj", "ezendmm", "equal","2")) # test if first stop does work (condition variable not in df)
  expect_error(af_test_simple(test_data, "intm", "intj", "ezendmm", "equals","2")) # test if false conditional operator argument errors
  expect_error(af_test_simple(test_data, "intm", "intj", "ezendmm", "equals","two")) # test if string conditional value argument errors
  expect_error(af_test_simple(test_data, "intm", "intj", "ezendmm", "equal","2")) # test if first stop does work (condition variable not in df)
  expect_error(af_test_complex(test_data, "indm", "intj")) # test if first stop does work (var1 variable not in df)
  expect_error(af_test_complex(test_data, "intm", "indj")) # test if second stop does work (var2 variable not in df)
})


test_that("af_test_complex: test main feature of function and stops", {
  expect_no_error(af_test_complex(test_data, "ezendj", "ezmod","eziz")) # test if it throws no error when we pass valid  test_var, target_var, overfiltered_vars values according to pv af-tag
  expect_error(af_test_complex(test_data, "ezendm", "ezmod","eziz")) # test if it throws an error when we pass non-valid  test_var, target_var, overfiltered_vars values according to pv af-tag. ezit should always be NA here but is not, so we expect an error
  expect_error(af_test_complex(test_data, "ezendm", "ezmod","eziz", "ezendmm", "equal","2")) # test if first stop does work (condition variable not in df)
  expect_error(af_test_complex(test_datas, "ezendm", "ezmod","eziz", "ezendmm", "equal","2")) # test if it errors when non-existent data argument is passed
  expect_error(af_test_complex(test_data, "ezendmm", "ezmod","eziz")) # test if second stop does work (test_var variable not in df)
  expect_error(af_test_complex(test_data, "ezendmm", "ezmod","eziz")) # test if second stop does work (test_var variable not in df)
  expect_error(af_test_complex(test_data, "ezendm", "ezmod","")) # test if third stop does work (A: overfiltered_vars argument not empty )
  expect_error(af_test_complex(test_data, "ezendm", "ezmod",list(a = "test"))) # test if third stop does work (B: overfiltered_vars argument is vector)
  expect_error(af_test_complex(test_data, "ezendm", "ezmod",)) # test if forth stop does work (target_var has any NA values)
  expect_error(af_test_complex(test_data, "ezendm", "ezmod",c("kieubeeg","esmod"))) # test if fifth stop does work (overfiltered vars exist)
})
