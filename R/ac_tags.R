
#' Tests if target variable equals target value
#'
#' @description This function is designed to test ac-tags. It takes a dataset in data argument and tests if the variable in test_var argument equals the value or the variables value in argument target_var.
#'
#' @param data A neps field-data dataframe.
#' @param target_var A string that represents a variable within the dataframe.
#' @param target_value A string that can either be a digit or a variable name present in the dataframe.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include: "equal", "unequal", "greater", "smaller", "greaterequal", "smallerequal", and "inlist".
#' @param condition_value A string that contains a digit to be used as the conditional value.
#'
#' @export ac_test
ac_test <- function(data, target_var, target_value, condition_var = NULL, operator = "equal", condition_value = NULL) {
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

  # Function to reorder columns: caseid first, target_var second, then others. We do this because we print dataframe snippets in order to get a quick glance at different relevant variables in case of errors.
  reorder_cols <- function(df) {
    others <- setdiff(names(df), c(caseid_var, target_var))
    df[, c(caseid_var, target_var, others), drop = FALSE]
  }

   # 1. USE CASE: Numeric target_var specification
  if (is.numeric(target_value)) {
    # Rows where values are NOT in target_value or NA:
    fail_rows <- data[!(data[[target_var]] %in% target_value | is.na(data[[target_var]])), ]

    # TEST SUCCESS: If there are no Errors, print success message
    if(nrow(fail_rows) == 0) {
      message(paste0("SUCCESS: The '", target_var, "' variable contains only values in '", paste(target_value, collapse = ", "), "'"))
    }

      # TEST FAIL: if there are Errors, make a df with failing caseids and then print Error message with these caseids and print df with these caseids
     else if(nrow(fail_rows) > 0) {

      fail_caseids <- unique(fail_rows[[caseid_var]])
      message("ERROR: Target variable '", target_var, "' contains unexpected values.")
      print(reorder_cols(fail_rows))

      # after the output, we stop the function and the script execution
      stop(paste0("Problematic caseid(s): ", paste(fail_caseids, collapse = ", ")))
     }
  }

  # 2. USE CASE: Character target_var specification (user supplies variable instead of value)
  else if (is.character(target_value)) {
    # first, check if the supplied character is avaiable as a var in data, if not, break function/script
    if (!(target_value %in% names(data))) {
      stop(paste("ERROR: Target value' ", target_value, "' not found as a variable in the dataframe"))
    }
    # here we collect cases that have not either NA values on both, target_var and target_value or are not equal
    fail_idx <- !((is.na(data[[target_var]]) & is.na(data[[target_value]])) |
                     (data[[target_var]] == data[[target_value]]) )

    # TEST SUCCESS: If there are no Errors, print success message
    if (!any(fail_idx)) {
      message(paste0("SUCCESS: The '", target_var, "' variable equals variable '", target_value, "'"))
    }

    # TEST FAIL: if there are Errors, make a df with failing caseids and then print Error message with these caseids and print df with these caseids
    else {
      fail_rows <- data[fail_idx, ]
      fail_caseids <- unique(fail_rows[[caseid_var]])
      message("ERROR: Target Variable '", target_var, "' does NOT match variable '", target_value, "'. Rows with failures: ")
      print(reorder_cols(fail_rows))
      stop(paste0("Problematic caseid(s): ", paste(fail_caseids, collapse = ", ")))
    }
  }

  #  3. "USE" CASE: Wrong specification of target_value argument
  else {
    stop("target_value must be either numeric or a character variable name in data.")
  }
}


