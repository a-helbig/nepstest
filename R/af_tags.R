
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
#' @description
#' This function is designed for testing af-tags that filter from one item to the following item (under condition x).
#' It checks that variables `var1` and `var2` have identical NA patterns, optionally under specified conditions.
#' If any discrepancies are found, it prints the problematic cases and stops execution.
#'
#' @param data A neps field-data dataframe.
#' @param var1 A string that represents the variable where we want to test the af-tag.
#' @param var2 A string that represents the variable where the af-tag is pointing to. Usually the following item.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include:
#'   \code{"equal"}, \code{"unequal"}, \code{"greater"}, \code{"smaller"},
#'   \code{"greaterequal"}, \code{"smallerequal"}, and \code{"inlist"}.
#' @param condition_value A string or numeric value to be used as the conditional value.
#' @param empty_as_na Logical, if TRUE empty strings ("") in character variables will be treated as NA. Default is TRUE.
#' @param print_filter_vars Optional character vector. Additional variable names to be included and prioritized in the printed output
#'   when errors are detected. This is useful for making filtered or relevant variables visible in diagnostic printouts.
#'
#' @return Invisibly returns the (possibly subsetted) data if NA patterns are identical or stops with an error if discrepancies found.
#'
#' @examples
#' \dontrun{
#' # Basic usage checking var1 and var2 equality of NA presence
#' af_test_simple(mydata, "var1", "var2")
#'
#' # With conditional subsetting and custom operator
#' af_test_simple(mydata, "var1", "var2", condition_var = "group", operator = "equal", condition_value = 1)
#'
#' # Including additional variables to print on error for visibility
#' af_test_simple(mydata, "var1", "var2", print_filter_vars = c("filter_var1", "filter_var2"))
#' }
#'
#' @export af_test_simple
af_test_simple <- function(data, var1, var2, condition_var = NULL, operator = "equal", condition_value = NULL, empty_as_na = TRUE, print_filter_vars = NULL) {
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

  # Check that if condition_var is provided, condition_value must also be provided
  if (!is.null(condition_var) && is.null(condition_value)) {
    stop("ERROR: condition_value must be provided when condition_var is specified.")
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

    # Validate print_filter_vars exist in the data columns
    valid_filter_vars <- print_filter_vars[print_filter_vars %in% names(print_data)]

    # Build cols_order: caseid, var1, var2, then print_filter_vars, then the rest
    cols_order <- c("caseid", var1, var2)

    # Append print_filter_vars that are not already in cols_order to avoid duplicates
    if (!is.null(valid_filter_vars)) {
      cols_order <- c(cols_order, setdiff(valid_filter_vars, cols_order))
    }

    # Append the rest of the columns, excluding those already included
    cols_order <- c(cols_order, setdiff(names(print_data), cols_order))

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
#' @description
#' This function is designed for testing af-tags that filter from one item to another by skipping items in between (under condition x).
#' It tests for NA equality between \code{test_var} and \code{target_var} and additionally whether all variables in \code{overfiltered_vars} are NA.
#' If any errors occur, it prints the problematic cases with specified extra variables if requested.
#'
#' @param data A neps field-data dataframe.
#' @param test_var A string representing the variable where we want to test the af-tag.
#' @param target_var A string representing the variable where the af-tag is pointing to.
#' @param overfiltered_vars A string or character vector of variables that must be NA as part of the skipped items.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string specifying the operator to be used for the condition. Acceptable operators include:
#'   \code{"equal"}, \code{"unequal"}, \code{"greater"}, \code{"smaller"},
#'   \code{"greaterequal"}, \code{"smallerequal"}, and \code{"inlist"}.
#' @param condition_value A string or numeric value to be used as the conditional value.
#' @param empty_as_na Logical, if TRUE empty strings ("") in character variables will be treated as NA. Default is TRUE.
#' @param print_filter_vars Optional character vector of variable names to be included and prioritized in
#'  the printed output when errors occur. Useful for showing filtered or relevant variables clearly.
#'
#' @return Invisibly returns the (possibly subsetted) data if all tests pass or stops with an error detailing problematic cases.
#'
#' @export af_test_complex
af_test_complex <- function(data, test_var, target_var, overfiltered_vars, condition_var = NULL,
                            operator = "equal", condition_value = NULL, empty_as_na = TRUE,
                            print_filter_vars = NULL) {
  data_name <- deparse(substitute(data))

  # Helper to replace "" with NA if requested
  replace_empty_with_na <- function(x, empty_as_na) {
    if (empty_as_na && is.character(x)) {
      x[x == ""] <- NA
    }
    x
  }

  # test_var and target_var should NOT be in overfiltered_vars
  if (any(c(test_var, target_var) %in% overfiltered_vars)) {
    stop("ERROR: Variables specified in test_var or target_var must not be included in overfiltered_vars.")
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

  data[[test_var]] <- replace_empty_with_na(data[[test_var]], empty_as_na)

  # Keep only rows where test_var is not NA
  data <- data[!is.na(data[[test_var]]), ]

  if (!target_var %in% names(data)) {
    stop(paste("ERROR: target_var", target_var, "not found in the dataframe"))
  }

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

  ### Test 1: Check all overfiltered_vars are NA or duration vars are 0 or NA (if duration vars exist)
  test1_fail_caseids <- character(0)
  test1_fail_rows <- integer(0)
  test1_success <- TRUE
  for (var in overfiltered_vars) {
    # consider both "variableduration" and "src_variableduration"
    duration_candidates <- c(paste0(var, "duration"), paste0("src_", var, "duration"))

    duration_var <- duration_candidates[duration_candidates %in% names(data)]

    if (length(duration_var) > 0) {
      # prefer the non-src variant if both exist
      if (length(duration_var) > 1 && duration_candidates[1] %in% duration_var) {
        duration_var <- duration_candidates[1]
      } else {
        duration_var <- duration_var[1]
      }
      # fail if var not NA AND duration exists AND duration is non-NA AND duration != 0
      test1_fails <- which(!is.na(data[[var]]) & (!is.na(data[[duration_var]]) & data[[duration_var]] != 0))
    } else {
      # no duration var found: fail if var not NA
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
    message(paste0("Test 1: ERROR - not all overfiltered vars are NA under condition specified. Please inspect caseid(s): ", paste(test1_fail_caseids, collapse = ", ")))

    print_data1 <- data[test1_fail_rows, , drop = FALSE]

    # Validate print_filter_vars exist in print_data1
    valid_filter_vars <- print_filter_vars[print_filter_vars %in% names(print_data1)]

    # Compose column order with prioritization of print_filter_vars
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars)
    if (!is.null(valid_filter_vars)) {
      cols_order <- c(cols_order, setdiff(valid_filter_vars, cols_order))
    }
    cols_order <- c(cols_order, setdiff(names(print_data1), cols_order))

    print_data1 <- print_data1[, cols_order, drop = FALSE]

    print(print_data1)
  } else {
    message("Test 1: SUCCESS - all overfiltered vars are NA.")
  }

  ### Test 2: Check target_var NA equivalence with test_var, only for non-NA test_var rows
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
                   "'. NA mismatches where test_var is not NA. Please inspect caseid(s): ", paste(test2_fails_caseids, collapse = ", "), "."))

    print_data2 <- data[test2_fails, , drop = FALSE]

    # Validate print_filter_vars exist in print_data2
    valid_filter_vars <- print_filter_vars[print_filter_vars %in% names(print_data2)]

    # Compose column order with prioritization of print_filter_vars
    cols_order <- c("caseid", test_var, target_var, overfiltered_vars)
    if (!is.null(valid_filter_vars)) {
      cols_order <- c(cols_order, setdiff(valid_filter_vars, cols_order))
    }
    cols_order <- c(cols_order, setdiff(names(print_data2), cols_order))

    print_data2 <- print_data2[, cols_order, drop = FALSE]

    print(print_data2)
  } else {
    message("Test 2: SUCCESS - No NA mismatch between test_var and target_var where test_var is not NA.")
  }

  # Stop if either test failed
  if (length(test1_fail_caseids) > 0 || length(test2_fails_caseids) > 0) {
    stop("Either Test 1 or Test 2 failed or both. Please check problematic caseids manually.")
  }

  invisible(data)
}
