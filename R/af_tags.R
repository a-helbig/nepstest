
subset_data <- function(data, condition_var, condition_value, operator){
  # Subset the data based on conditions if supplied
  operator_map <- list(
    equal = `==`,
    unequal = `!=`,
    greater = `>`,
    smaller = `<`,
    greaterequal = `>=`,
    smallerequal = `<=`,
    inlist = `%in%`
  )

  # Check if the operator is valid
  if (!(operator %in% names(operator_map))) {
    stop("ERROR: Invalid operator argument.")
  }

  # Apply evaluation of dynamic expression
  selected_conditional <- operator_map[[operator]]
  data <- data[data[[condition_var]] |>  selected_conditional(condition_value) & !is.na(data[[condition_var]] |>  selected_conditional(condition_value)), ] # subset to conditional_value and no NA
  return(data)
}

#' Tests for NA equality between var1 and var2
#'
#' @description This function is designed for testing af-tags that filter from one item to the following item (under condition x).
#'
#' @param data A neps field-data dataframe.
#' @param var1 A string that represents the variable where we want to test the af-tag.
#' @param var2 A string that represents the variable where the af-tag is pointing too. Usually the following item.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include: "equal", "unequal", "greater", "smaller", "greaterequal", "smallerequal", and "inlist".
#' @param condition_value A string that contains a digit to be used as the conditional value.
#'
#' @export af_test_simple
af_test_simple <- function(data, var1, var2, condition_var = NULL, operator = "equal", condition_value = NULL) {
  data_name <- deparse(substitute(data))

  # Check variable presence
  if (!var1 %in% names(data)) {
    stop(paste("ERROR: Variable", var1, "not found in the dataframe", data_name))
  }
  if (!var2 %in% names(data)) {
    stop(paste("ERROR: Variable", var2, "not found in the dataframe", data_name))
  }

  # Subset if conditions are specified
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe."))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  var1_values <- data[[var1]]
  var2_values <- data[[var2]]

  # main function to test na equivalence between specified var1 and var2
  check_na_equivalence <- function(x, y) {
    (!is.na(x)) == (!is.na(y))
  }

  # which ids dont have NA-equivalence
  fail_idx <- !check_na_equivalence(var1_values, var2_values)

  # if there are any errors, print error message with a list of caseids and print informative dataframe with these caseids (reorder relevant variables) then stop func and script execution
  if (any(fail_idx)) {
    fail_caseids <- data$caseid[fail_idx]
    id_info <- paste("Problematic caseid(s):", paste(unique(fail_caseids), collapse = ", "))

    print_data <- data[fail_idx, ]
    # reorder col names in dataframe so that relevant variables are shown in the first cols. the setdiff here is used to print all other vars in df behind the first 3
    cols_order <- c("caseid", var1, var2, setdiff(names(print_data), c("caseid", var1, var2)))
    print_data <- print_data[, cols_order, drop = FALSE]

    message("ERROR: Unequal NA presence between '", var1, "' and '", var2, "'.")
    message(id_info)
    print(print_data)

    stop("Please inspect the above rows for NA mismatches.")
  }

  # if there are no errors, print sucess messagess
  else {
    message("SUCCESS: Variables '", var1, "' and '", var2, "' have identical NA patterns (under specified conditions).")
  }
}






#' Tests complex af-tag filters
#'
#' @description This function is designed for testing af-tags that filter from one item to another item by skipping the items in between (under condition x).
#' It tests for NA equality between test_var and target_var and additionally if all vars in overfiltered_vars are NA.
#'
#' @param data A neps field-data dataframe.
#' @param test_var A string that represents the variable where we want to test the af-tag.
#' @param target_var A string that represents the variable where the af-tag is pointing too.
#' @param overfiltered_vars A string containing a single variable or a character vector of variables that will be skipped according to the af-tag and must be NA.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include: "equal", "unequal", "greater", "smaller", "greaterequal", "smallerequal", and "inlist".
#' @param condition_value A string that contains a digit to be used as the conditional value.
#'
#' @export af_test_complex
af_test_complex <- function(data, test_var, target_var, overfiltered_vars, condition_var = NULL, operator = "equal", condition_value = NULL) {
  data_name <- deparse(substitute(data))

  # Subset data if condition specified
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Check presence of test_var and exclude NA rows there
  if (!test_var %in% names(data)) {
    stop(paste("ERROR: test_var", test_var, "not found in the dataframe"))
  }
  data <- data[!is.na(data[[test_var]]), ]

  # Check presence of target_var
  if (!target_var %in% names(data)) {
    stop(paste("ERROR: target_var", target_var, "not found in the dataframe"))
  }

  # Check overfiltered_vars is a non-empty vector
  if (!is.vector(overfiltered_vars) || length(overfiltered_vars) == 0) {
    stop("ERROR: overfiltered_vars must be a non-empty vector of variable names.")
  }

  # Check all overfiltered_vars exist in data
  missing_vars <- overfiltered_vars[!overfiltered_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("ERROR: The following overfiltered_vars not found in dataset:", paste(missing_vars, collapse = ", ")))
  }

  ### Run Test 1: Check all overfiltered_vars are NA
  test1_fail_caseids <- character(0)
  test1_success <- TRUE
  for (var in overfiltered_vars) {
    test1_fails <- which(!is.na(data[[var]]))
    if (length(test1_fails) > 0) {
      test1_success <- FALSE
      test1_fail_caseids <- c(test1_fail_caseids, unique(data$caseid[test1_fails]))
    }
  }
  # reduce to unique caseids
  test1_fail_caseids <- unique(test1_fail_caseids)

  # print a message with all caseids where overfiltered_vars were not NA
  if(!test1_success) {
    message(paste0("Test 1: ERROR - not all overfiltered vars are NA under condition specified. Please inspect the caseid(s):", paste(test1_fail_caseids, collapse = ", ")))

    print_data1 <- data[test1_fails, ]
    # reorder col names in dataframe so that relevant variables are shown in the first cols. the setdiff here is used to print all other vars in df behind the first 3
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars, setdiff(names(print_data1), c("caseid", test_var, target_var, overfiltered_vars)))
    # now we use this vec with the desired order of variables to actually sort the df
    print_data1 <- print_data1[, cols_order]
    print(print_data1)
  }

  else message("Test 1: SUCCESS - all overfiltered var are NA.")

  ### Run Test 2: Check target vars are
  # initialize vars
  test2_fail_caseids <- character(0)
  test2_success <- TRUE

  # define function for testing na equivalence between 2 vars in df
  check_na_equivalence <- function(x, y) (!is.na(x)) == (!is.na(y))

  # create logical vector on NA-equivalence
  test2_fails <- !check_na_equivalence(data[[test_var]], data[[target_var]])
  # use this to subset caseid vector
  test2_fails_caseids <- unique(data$caseid[test2_fails])
  # set flag for success = TRUE when there are no caseids
  test2_success <- length(test2_fails_caseids) == 0

  if(!test2_success)
  {
    message(paste0("Test 2: ERROR - Target-Variable '", target_var, "' has NA mismatch with Test-Variable '", test_var, "'. Please inspect the caseid(s): ", paste(test2_fails_caseids, collapse = ", "),"."))

    print_data2 <- data[test2_fails, ]
    # reorder col names in dataframe so that relevant variables are shown in the first cols. the setdiff here is used to print all other vars in df behind the first 3
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars, setdiff(names(print_data2), c("caseid", test_var, target_var, overfiltered_vars)))
    # now we use this vec with the desired order of variables to actually sort the df
    print_data2 <- print_data2[, cols_order]
    print(print_data2)
  }
  else message("Test 2: SUCCESS - No NA mismatch between test_var and target_var.")

  # stop function of any of both tests failed. That means of any overfiltered vars have non NA values or if target var has NA mismatch with test variable (under specified condition)
  if(length(test1_fail_caseids)>0 | length(test2_fail_caseids)>0) stop("Either Test 1 or Test 2 failed or both. Please check problematic caseids manually.")

}
