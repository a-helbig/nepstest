
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
#' @param empty_as_na Logical, if TRUE empty strings ("") in character variables will be treated as NA. Default is TRUE
#'
#' @export af_test_simple
af_test_simple <- function(data, var1, var2, condition_var = NULL, operator = "equal", condition_value = NULL, empty_as_na = TRUE) {
  data_name <- deparse(substitute(data))

  # Helper to replace "" with NA in character vectors if requested
  replace_empty_with_na <- function(x, empty_as_na) {
    if (empty_as_na && is.character(x)) {
      x[x == ""] <- NA
    }
    x
  }

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

  # replace empty strings "''" with NA.
  var1_values <- replace_empty_with_na(data[[var1]], empty_as_na)
  var2_values <- replace_empty_with_na(data[[var2]], empty_as_na)

  # which ids dont have NA-equivalence
  not_na_var1 <- !is.na(var1_values) # get rid of cases where var1 is NA
  fail_idx <- not_na_var1 & is.na(var2_values)

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

  # if there are no errors, print success messages
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
#' @param empty_as_na Logical, if TRUE empty strings ("") in character variables will be treated as NA. Default is TRUE
#'
#' @export af_test_complex
af_test_complex <- function(data, test_var, target_var, overfiltered_vars, condition_var = NULL,
                            operator = "equal", condition_value = NULL, empty_as_na = TRUE) {
  data_name <- deparse(substitute(data))

  # Helper that replaces empty strings with NA in character vectors if flag is TRUE
  replace_empty_with_na <- function(x, empty_as_na) {
    if (empty_as_na && is.character(x)) {
      x[x == ""] <- NA
    }
    x
  }

  # Subset data if condition specified
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Check presence of test_var
  if (!test_var %in% names(data)) {
    stop(paste("ERROR: test_var", test_var, "not found in the dataframe"))
  }

  # Replace empty strings with NA in test_var, target_var, and overfiltered_vars
  data[[test_var]] <- replace_empty_with_na(data[[test_var]], empty_as_na)

  # Subset rows where test_var is not NA (same as original behavior)
  data <- data[!is.na(data[[test_var]]), ]

  if (!target_var %in% names(data)) {
    stop(paste("ERROR: target_var", target_var, "not found in the dataframe"))
  }

  # replace empty strings "''" with NA.
  data[[target_var]] <- replace_empty_with_na(data[[target_var]], empty_as_na)

  data[[test_var]] <- replace_empty_with_na(data[[test_var]], empty_as_na)

  for (var in overfiltered_vars) {
    data[[var]] <- replace_empty_with_na(data[[var]], empty_as_na)
  }

  if (!is.vector(overfiltered_vars) || length(overfiltered_vars) == 0) {
    stop("ERROR: overfiltered_vars must be a non-empty vector of variable names.")
  }

  missing_vars <- overfiltered_vars[!overfiltered_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("ERROR: The following overfiltered_vars not found in dataset:", paste(missing_vars, collapse = ", ")))
  }

  ### Run Test 1: Check all overfiltered_vars are NA or the corresponding duration variable is 0 (that was changed in order to tackle the issue with autocodes of specific variables). The latter will only be checked when a corresponding duration variable exists
  test1_fail_caseids <- character(0)
  test1_fail_rows <- integer(0)
  test1_success <- TRUE
  for (var in overfiltered_vars) {
    duration_var <- paste0(var, "duration") # create the duration pendant

    if (duration_var %in% names(data)) {
      # Duration var exists: fail if var not NA AND (duration var is NA or != 0)
      test1_fails <- which(!is.na(data[[var]]) & (is.na(data[[duration_var]]) | data[[duration_var]] != 0))
    } else {
      # Duration var missing: fail if var is not NA (original test)
      test1_fails <- which(!is.na(data[[var]]))
    }

    if (length(test1_fails) > 0) {
      test1_success <- FALSE
      test1_fail_caseids <- c(test1_fail_caseids, unique(data$caseid[test1_fails]))
      test1_fail_rows <- c(test1_fail_rows, test1_fails)
    }
  }
  test1_fail_caseids <- unique(test1_fail_caseids)
  test1_fail_rows <- unique(test1_fail_rows)

  if (!test1_success) {
    message(paste0("Test 1: ERROR - not all overfiltered vars are NA under condition specified. Please inspect the caseid(s): ", paste(test1_fail_caseids, collapse = ", ")))

    print_data1 <- data[test1_fail_rows, , drop = FALSE]
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars, setdiff(names(print_data1), c("caseid", test_var, target_var, overfiltered_vars)))
    print_data1 <- print_data1[, cols_order]
    print(print_data1)
  } else message("Test 1: SUCCESS - all overfiltered var are NA.")

  ### Run Test 2: Check target vars NA equivalence with test_var, only for non-NA test_var rows
  not_na_test_var <- !is.na(data[[test_var]])

  check_na_equivalence <- function(x, y) {
    fail_vec <- rep(FALSE, length(x))
    fail_vec[not_na_test_var] <- is.na(y[not_na_test_var])
    fail_vec
  }

  test2_fails <- check_na_equivalence(data[[test_var]], data[[target_var]])
  test2_fails_caseids <- unique(data$caseid[test2_fails])
  test2_success <- length(test2_fails_caseids) == 0

  if (!test2_success) {
    message(paste0("Test 2: ERROR - Target-Variable '", target_var, "' has NA mismatch with Test-Variable '", test_var,
                   "'. NA mismatches where test_var is not NA. Please inspect the caseid(s): ", paste(test2_fails_caseids, collapse = ", "), "."))

    print_data2 <- data[test2_fails, , drop = FALSE]
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars, setdiff(names(print_data2), c("caseid", test_var, target_var, overfiltered_vars)))
    print_data2 <- print_data2[, cols_order]
    print(print_data2)
  } else message("Test 2: SUCCESS - No NA mismatch between test_var and target_var where test_var is not NA.")

  if (length(test1_fail_caseids) > 0 || length(test2_fails_caseids) > 0) {
    stop("Either Test 1 or Test 2 failed or both. Please check problematic caseids manually.")
  }
}
