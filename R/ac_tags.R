
#' Tests if target variable equals target value
#'
#' This function is designed to test ac-tags. It takes a dataset in data argument and tests if the variable in test_var argument equals the value or the variables value in argument target_var.
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
  # Check if variable and condition_var exist in the dataframe
  if (!target_var %in% names(data)) {
    stop(paste("ERROR: Variable", target_var, "not found in the dataframe"))
  }

  # Subset the data based on conditions if supplied
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Initialize the only_x variable
  only_x <- NULL

  # Check if target_value is numeric
  if (is.numeric(target_value)) {
    if (target_var %in% names(data)) {
      # Check if all values in target_var are one of the values in target_value or NA
      only_x <- all(data[[target_var]] %in% target_value | is.na(data[[target_var]]))
      if (only_x) {
        print(paste("Success!! The '", target_var, "' variable contains only values in '", paste(target_value, collapse = ", "), "' under specified condition")) # Feedback in case target_value is numeric
      }
      else {
        stop(paste("Error!! The '", target_var, "' variable does NOT contain only values in '", paste(target_value, collapse = ", "), "'")) # Error Message
      }
    }

    # Check if target_value is a character that matches a variable in the data
  } else if (is.character(target_value)) {
    # Check if target_value is a valid variable in the dataframe
    if (!(target_value %in% names(data))) {
      stop(paste("ERROR: Target value", target_value, "not found as a variable in the dataframe"))
    }

    # Check if both target_var and target_value are NA
    only_x <- all((is.na(data[[target_var]]) & is.na(data[[target_value]])) |
                    (data[[target_var]] == data[[target_value]]))

    # Print messages based on the result and stop script if values are not confirmed
    if (is.na(only_x)) {
      stop("Error, there are NA values that do not align across the two variables")
    } else if (only_x) {
      print(paste("Success!! The '", target_var, "' variable equals variable '", target_value, "' under specified condition"))
    } else {
      stop(paste("Error!! The '", target_var, "' variable does NOT contain only '", target_value, "' values"))
    }
  }
}

