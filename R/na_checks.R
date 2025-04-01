#' Tests if there are only NA values in specified var_names
#'
#' @param data A neps field-data dataframe.
#' @param var_names A character vector with variables that will be checked to ensure they contain only NA values (under condition x).
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include: "equal", "unequal", "greater", "smaller", "greaterequal", "smallerequal", and "inlist".
#' @param condition_value A string that contains a digit to be used as the conditional value.
#'
#' @export only_na_in_vars
only_na_in_vars <- function(data, var_names, condition_var=NULL, operator = "equals", condition_value=NULL) {

  # Subset the data based on conditions if supplied
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Check if the specified variables are NA in the filtered data
  na_checks <- sapply(var_names, function(var) {
    all(is.na(data[[var]]))
  })

  if (any(!na_checks)) {
    stop("ERROR: Not all specified variables are NA under the given condition.")
  } else {
    print("Success: All specified variables are NA under the given condition.")
  }
}

#' Tests if there are no NA values in var_names
#'
#' @description Most commonly used for testing a bunch of preload variables not being NA in case of "Aufsatzepisoden".
#'
#' @param data A neps field-data dataframe.
#' @param var_names A character vector with variables that will be checked to ensure there are no NA values.
#' @param condition_var A string representing a variable name to be used as a conditional variable.
#' @param operator A string that specifies the operator to be used for the condition. Acceptable operators include: "equal", "unequal", "greater", "smaller", "greaterequal", "smallerequal", and "inlist".
#' @param condition_value A string that contains a digit to be used as the conditional value.
#'
#' @export no_na_in_vars
no_na_in_vars <- function(data, var_names, condition_var=NULL, operator = "equal", condition_value=NULL) {
  # Ensure that var_names is a vector and not empty
  if (!is.vector(var_names) || length(var_names) == 0) {
    stop("ERROR: var_names must be a non-empty vector of variable names.")
  }

  # Check if all specified variables exist in the dataframe
  for (var in var_names) {
    if (!var %in% names(data)) {
      stop(paste("ERROR: Variable", var, "not found in the dataframe"))
    }
  }

  # Subset the data based on conditions if supplied
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Initialize a vector to store names of variables with NAs
  vars_with_na <- character(0)

  # Check each specified variable for NAs
  for (var in var_names) {
    if (any(is.na(data[[var]]))) {
      vars_with_na <- c(vars_with_na, var)
    }
  }

  # Return results
  if (length(vars_with_na) > 0) {
    stop(paste("Error: Variables with NA values:", paste(vars_with_na, collapse = ", ")))

  } else {
    print("No NA values found in the specified variables.")
  }
}
