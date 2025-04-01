
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

  # Capture the name of the data argument
  data_name <- deparse(substitute(data))

  # Check if var1 and var2 exist in the dataframe
  if (!var1 %in% names(data)) {
    stop(paste("ERROR: Variable", var1, "not found in the dataframe", data_name))
  }

  if (!var2 %in% names(data)) {
    stop(paste("ERROR: Variable", var2, "not found in the dataframe", data_name))
  }

  # Subset the data based on conditions if supplied
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Extract variable values
  var1_values <- data[[var1]]
  var2_values <- data[[var2]]

  # Function to check NA equivalence
  check_na_equivalence <- function(x, y) {
    !is.na(x) == !is.na(y)
  }

  # Check for unequal NA values
  if (!all(check_na_equivalence(var1_values, var2_values))) {
    stop(paste("ERROR, unequal NA values between", var1, "and", var2, "in dataset", data_name))
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
  # Capture the name of the data argument
  data_name <- deparse(substitute(data))

  # Subset the data based on condition_var and condition_value if supplied
  if (!is.null(condition_var) && !is.null(condition_value)) {
    if (!condition_var %in% names(data)) {
      stop(paste("ERROR: Condition variable", condition_var, "not found in the dataframe"))
    }
    data <- subset_data(data, condition_var, condition_value, operator)
  }

  # Subset the data where test_var is not NA
  if (!test_var %in% names(data)) {
    stop(paste("ERROR: test_var", test_var, "not found in the dataframe"))
  }
  data <- data[!is.na(data[[test_var]]), ]

  # Pull out values for target_var
  target_var_values <- data[[target_var]]

  # Ensure argument overfiltered_vars is a vector and not empty
  if (!is.vector(overfiltered_vars) || length(overfiltered_vars) == 0) {
    stop("ERROR: overfiltered_vars must be a non-empty vector of variable names.")
  }

  # Check if target_var has any NA values
  if (any(is.na(target_var_values))) {
    stop(paste("ERROR: Target variable", target_var, "contains NA values in dataset", data_name))
  }

  # Iterate over the variable names in overfiltered_vars
  for (var in overfiltered_vars) {
    # Check whether the variable exists in the data frame
    if (!var %in% names(data)) {
      stop(paste("ERROR: Variable", var, "not found in dataset", data_name))
    }

    overfiltered_vars_values <- data[[var]]  # Correctly pull out the column for each var in overfiltered_vars

    # Check if the corresponding overfiltered_vars_values is not NA
    if (any(!is.na(overfiltered_vars_values))) {
      stop(paste("ERROR: Overfiltered variable", var, "must be NA to fulfill af-tag condition"))
    }
  }
}
