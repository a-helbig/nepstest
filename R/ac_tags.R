#' Test if a target variable equals a specified value or another variable's value
#'
#' @description
#' This function is designed to test ac-tags by comparing values in a dataframe.
#' It takes a dataframe as input and tests whether the variable given by `target_var`
#' equals either a fixed value or the values of another variable in the dataframe (`target_value`).
#' Optionally, a conditional subset of the data can be specified to restrict the test.
#'
#' The function reports success if all tested rows meet the condition.
#' If any rows do not meet the condition, it prints case IDs and relevant variables (including those specified
#' in `print_filter_vars`) for debugging, then stops with an error.
#'
#' @param data A dataframe (typically a neps field-data dataframe).
#' @param target_var A string specifying the name of the variable to test in the dataframe.
#' @param target_value Either:
#'   - a numeric vector of expected values, or
#'   - a string specifying the name of another variable in the dataframe whose values are compared against `target_var`.
#' @param condition_var Optional string specifying a variable name to subset/filter the data before testing.
#' @param operator Optional string specifying the comparison operator used for subsetting. One of:
#'   `"equal"`, `"unequal"`, `"greater"`, `"smaller"`, `"greaterequal"`, `"smallerequal"`, or `"inlist"`.
#'   Defaults to `"equal"`.
#' @param condition_value Optional numeric or character value used in conjunction with `condition_var` and `operator`
#'   to subset the data before testing.
#' @param print_filter_vars Optional character vector specifying additional variable names whose columns should be prioritized
#'   and displayed early in error prints to assist debugging (e.g. variables used in `filter()` prior to calling `ac_test`).
#'
#' @return Invisibly returns NULL. Function either prints a success message or stops with a detailed error.
#'
#' @details
#' The function expects `caseid` column in the data by default and always includes it in output.
#' When errors occur, output tables prioritize columns in the following order:
#' `caseid`, `target_var`, variables in `print_filter_vars`, then all other columns.
#'
#' @examples
#' \dontrun{
#' # Simple test that h_etumf equals 1
#' ac_test(mydata, target_var = "h_etumf", target_value = 1)
#'
#' # Test that h_etumf equals values of another variable h_expected
#' ac_test(mydata, target_var = "h_etumf", target_value = "h_expected")
#'
#' # Test on a subset of data where etazv is between 15 and 90, prioritizing etazv in error prints
#' filtered_data <- mydata %>% filter(etazv %in% c(15:90, -20))
#' ac_test(filtered_data, target_var = "h_etumf", target_value = 1,
#'         print_filter_vars = c("etazv"))
#' }
#'
#' @export
ac_test <- function(data, target_var, target_value, condition_var = NULL, operator = "equal", condition_value = NULL, print_filter_vars = NULL) {
  caseid_var <- "caseid"  # hardcoded default caseid column

  # Check required columns: target_var must be found as a variable name in data
  if (!target_var %in% names(data)) {
    stop(paste("ERROR: Variable", target_var, "not found in the dataframe"))
  }
  # Check required columns: caseid_var must be found as a variable name in data
  if (!caseid_var %in% names(data)) {
    stop(paste("ERROR: Case ID variable", caseid_var, "not found in the dataframe"))
  }

  # Subset data according to condition (same as your original)
  if (!is.null(condition_var) && !is.null(condition_value)) {
    # first, check if condition var is existent in df
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    # actual subsetting
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Check extra_vars exist
  if(!is.null(print_filter_vars)) {
    missing_vars <- setdiff(print_filter_vars, names(data))
    if(length(missing_vars) > 0) {
      stop(paste("ERROR: Filter variables not found:", paste(missing_vars, collapse = ", ")))
    }
  }

  # Function to reorder columns: caseid first, target_var second, then others. We do this because we print dataframe snippets in order to get a quick glance at different relevant variables in case of errors.
  reorder_cols <- function(df) {
    filter_vars <- if (!is.null(print_filter_vars)) {
      print_filter_vars[print_filter_vars %in% names(df)]
    } else {
      character(0)
    }
    others <- setdiff(names(df), c(caseid_var, target_var,filter_vars))
    df[, c(caseid_var, target_var, filter_vars, others), drop = FALSE]
  }

  print_fail_rows <- function(fail_rows) {
    df_print <- reorder_cols(fail_rows)
    # For wide data, show only a limited number of columns (say max 10 columns)
    max_cols_to_show <- 10
    cols_to_print <- head(names(df_print), max_cols_to_show)
    message(paste("Showing first", max_cols_to_show, "columns of failing rows (columns truncated if data is wider):"))
    print(df_print[, cols_to_print, drop = FALSE])
  }

  # 1. USE CASE: Numeric target_var specification
  if (is.numeric(target_value)) {
    # suppress labelled value label warnings here
    fail_rows <- suppressWarnings(
      data[!(data[[target_var]] %in% target_value | is.na(data[[target_var]])), ]
    )

    if (nrow(fail_rows) == 0) {
      message(paste0("SUCCESS: The '", target_var, "' variable contains only values in '", paste(target_value, collapse = ", "), "'"))
    } else if (nrow(fail_rows) > 0) {
      fail_caseids <- unique(fail_rows[[caseid_var]])
      message("ERROR: Target variable '", target_var, "' contains unexpected values.")
      print_fail_rows(fail_rows)
      stop(paste0("Problematic caseid(s): ", paste(fail_caseids, collapse = ", ")))
    }
  }

  # 2. USE CASE: Character target_var specification (user supplies variable instead of value)
  else if (is.character(target_value)) {
    if (!(target_value %in% names(data))) {
      stop(paste("ERROR: Target value' ", target_value, "' not found as a variable in the dataframe"))
    }

    equals <- suppressWarnings(data[[target_var]] == data[[target_value]])
    equals[is.na(equals)] <- FALSE

    both_na <- is.na(data[[target_var]]) & is.na(data[[target_value]])

    fail_idx <- !(both_na | equals)

    if (!any(fail_idx)) {
      message(paste0("SUCCESS: The '", target_var, "' variable equals variable '", target_value, "'"))
    } else {
      fail_rows <- data[fail_idx, ]
      fail_caseids <- unique(fail_rows[[caseid_var]])
      message("ERROR: Target Variable '", target_var, "' does NOT match variable '", target_value, "'. Rows with failures: ")
      print_fail_rows(fail_rows)
      stop(paste0("Problematic caseid(s): ", paste(fail_caseids, collapse = ", ")))
    }
  }

  #  3. "USE" CASE: Wrong specification of target_value argument
  else {
    stop("target_value must be either numeric or a character variable name in data.")
  }
}


