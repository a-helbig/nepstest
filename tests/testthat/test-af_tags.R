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





# -------- Tests for subset_data --------

# Mock subset_data for testing with conditions
subset_data <- function(data, condition_var, condition_value, operator) {
  operator_map <- list(
    equal = `==`,
    unequal = `!=`,
    greater = `>`,
    smaller = `<`,
    greaterequal = `>=`,
    smallerequal = `<=`,
    inlist = `%in%`
  )
  if (!(operator %in% names(operator_map))) {
    stop("ERROR: Invalid operator argument.")
  }
  f <- operator_map[[operator]]
  data[data[[condition_var]] |> f(condition_value) & !is.na(data[[condition_var]] |> f(condition_value)), ]
}


test_that("subset_data subsets correctly with 'equal' operator", {
  df <- data.frame(caseid = 1:5, var = c(1, 2, 2, NA, 1))
  res <- subset_data(df, "var", 1, "equal")
  expect_equal(nrow(res), 2)
  expect_true(all(res$var == 1))
})

test_that("subset_data subsets correctly with other operators", {
  df <- data.frame(caseid = 1:5, var = c(1, 2, 3, 4, NA))
  res <- subset_data(df, "var", 2, "greater")
  expect_equal(nrow(res), 2)
  expect_true(all(res$var > 2))

  res2 <- subset_data(df, "var", c(1,3), "inlist")
  expect_equal(nrow(res2), 2)
  expect_true(all(res2$var %in% c(1, 3)))
})

test_that("subset_data removes NA rows after applying condition", {
  df <- data.frame(caseid = 1:4, var = c(1, NA, 3, 4))
  res <- subset_data(df, "var", 3, "greater")
  # Only var>3 and not NA -> should keep var == 4 only
  expect_equal(nrow(res), 1)
  expect_equal(res$var, 4)
})

test_that("subset_data errors with invalid operator", {
  df <- data.frame(caseid = 1:3, var = 1:3)
  expect_error(subset_data(df, "var", 1, "invalid_op"),
               regexp = "Invalid operator argument")
})

# -------- Tests for af_test_simple --------

# Small helper df
df_simple <- data.frame(
  caseid = 1:5,
  v1 = c(1, NA, 3, NA, 5),
  v2 = c(10, NA, 30, NA, 50),
  cond = c(1, 1, 2, 2, 1),
  stringsAsFactors = FALSE
)

# Mock subset_data for testing with conditions
subset_data <- function(data, condition_var, condition_value, operator) {
  operator_map <- list(
    equal = `==`,
    unequal = `!=`,
    greater = `>`,
    smaller = `<`,
    greaterequal = `>=`,
    smallerequal = `<=`,
    inlist = `%in%`
  )
  if (!(operator %in% names(operator_map))) {
    stop("ERROR: Invalid operator argument.")
  }
  f <- operator_map[[operator]]
  data[data[[condition_var]] |> f(condition_value) & !is.na(data[[condition_var]] |> f(condition_value)), ]
}

test_that("af_test_simple succeeds if NA patterns are equal", {
  expect_message(
    af_test_simple(df_simple, "v1", "v2"),
    regexp = "SUCCESS"
  )
})

test_that("af_test_simple fails if NA patterns differ", {
  df_bad <- df_simple
  df_bad$v2[1] <- NA   # v2 had NA at index 2, now numeric -> mismatch
  expect_error(
    af_test_simple(df_bad, "v1", "v2")
  )
})

test_that("af_test_simple subsets correctly with condition", {
  # Only rows where cond == 1 are kept (rows 1,2,5)
  expect_message(
    af_test_simple(df_simple, "v1", "v2", condition_var = "cond", operator = "equal", condition_value = "1"),
    regexp = "SUCCESS"
  )
})

test_that("af_test_simple errors if var1, var2, or condition_var missing", {
  expect_error(af_test_simple(df_simple, "missing", "v2"), regexp = "Variable missing not found")
  expect_error(af_test_simple(df_simple, "v1", "missing"), regexp = "Variable missing not found")
  expect_error(af_test_simple(df_simple, "v1", "v2", condition_var = "missing", condition_value = "1"),
               regexp = "Condition variable missing not found")
})

# -------- Tests for af_test_complex --------

df_complex <- data.frame(
  caseid = 1:6,
  test_var = c(1, NA, 3, 4, NA, 6),
  target_var = c(999, NA, 3, 88, NA, 6),  # Non-NA where test_var non-NA (rows 1,3,4,6)
  over1 = rep(NA, 6),
  over2 = rep(NA, 6),
  cond = c(1, 1, 2, 2, 1, 1),
  stringsAsFactors = FALSE
)

test_that("af_test_complex succeeds when overfiltered_vars are NA and NA patterns match", {
  expect_no_error(
    af_test_complex(df_complex, "test_var", "target_var", c("over1", "over2")))
})

test_that("af_test_complex errors if overfiltered_vars contain non-NA values", {
  # Inject non-NA value in over1 for caseid 4
  df_bad <- df_complex
  df_bad$over1[4] <- 999
  expect_error(
    af_test_complex(df_bad, "test_var", "target_var", c("over1", "over2")),
    regexp = "Either Test 1 or Test 2 failed or both"
  )
})

test_that("af_test_complex subsets correctly with condition", {
  # Keep only cond == 1 -> rows 1,2,5,6
  expect_no_error(
    af_test_complex(df_complex, "test_var", "target_var", c("over1", "over2"), condition_var = "cond", operator = "equal", condition_value = "1")
  )
})

test_that("af_test_complex errors with missing variables", {
  expect_error(af_test_complex(df_complex, "missing", "target_var", c("over1")), regexp = "test_var missing not found")
  expect_error(af_test_complex(df_complex, "test_var", "missing", c("over1")), regexp = "target_var missing not found")
  expect_error(af_test_complex(df_complex, "test_var", "target_var", character(0)), regexp = "overfiltered_vars must be a non-empty vector")
  expect_error(af_test_complex(df_complex, "test_var", "target_var", c("missingvar")), regexp = "not found in dataset")
})

test_that("Ignores rows where var1 is NA even if var2 is NOT NA", {
  data <- data.frame(
    caseid = 1:4,
    var1 = c(NA, NA, "val1", "val2"),
    var2 = c("not_na", NA, "val1", NA),
    stringsAsFactors = FALSE
  )

  # Rows 1 and 2 have var1 NA, but var2 is one not NA, one NA -> both ignored
  # Rows 3,4: var1 non-NA, var2 for 3 is var1 non-NA also, for 4 var2 is NA -> row 4 triggers fail

  # So this should fail because row 4 fails the check (var1 not NA, var2 NA)
  expect_error(
    af_test_simple(data, "var1", "var2")
  )

  # Now a similar dataset but var2 fully present for var1 non-NA rows:
  data2 <- data.frame(
    caseid = 1:4,
    var1 = c(NA, NA, "val1", "val2"),
    var2 = c("not_na", NA, "val1", "val2"),
    stringsAsFactors = FALSE
  )

  # This one should pass - var1 NA rows ignored, all var1 non-NA rows match with var2 not NA
  expect_no_error(
    af_test_simple(data2, "var1", "var2")
  )
})

