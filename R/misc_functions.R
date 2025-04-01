#' Test if variables are existent in the data
#'
#' @description This function can be used to test if a bunch of Hilfsvariable are existent in dataframe where we usually dont have any filtering in the instruction sheets.
#' @param data A neps field-data dataframe.
#' @param vars A string vector with variable names. It will be checked if these exist in the dataframe.
#'
#' @export
var_exists <- function(data, vars) {
  # Capture the name of the data argument
  data_name <- deparse(substitute(data))

  # Initialize a vector to hold missing variables
  missing_vars <- c()

  for (var in vars) {
    if (!var %in% names(data)) {
      missing_vars <- c(missing_vars, var)
    }
  }

  # If there are any missing variables, stop and report them
  if (length(missing_vars) > 0) {
    stop(paste("Error, Variable(s)", paste(missing_vars, collapse = ", "),
               "is (are) missing in dataset:", data_name))
  } else {
    print(paste("Success: All specified variables are present in dataset:", data_name))
  }
}

#' Compare the values of two date items
#'
#' @description It compares two variables (month and year) of date item 1 with two variables of date item 2 (month and year).
#' Valid comparison operators are: 'smaller', 'smaller_equal', 'larger', 'larger_equal' and 'equal'.
#' Usually used to test ra-tags, where we compare two or more date variables against each other. The function mostly has to be used more than once on order to test a range tag completly.
#'
#' @param data A neps field-data dataframe.
#' @param varm1 A String that holds the name of a monthly date variable.
#' @param varj1 A String that holds the name of a yearly date variable.
#' @param comparison A String that holds a comparison operator. Valid Operators: 'smaller', 'smaller_equal', 'larger', 'larger_equal' and 'equal'.
#' @param varm2 A String that holds the name of a monthly date variable.
#' @param varj2 A String that holds the name of a monthly date variable.
#' @import utils
#' @export date_test
date_test <- function(data, varm1, varj1, comparison = "smaller", varm2, varj2) {

  # Check if all specified variables exist in the dataframe
  var_names <- c(varm1, varj1, varm2, varj2, "caseid")  # Include caseid in the checks
  for (var in var_names) {
    if (!var %in% names(data)) {
      stop(paste("ERROR: Variable", var, "not found in the dataframe"))
    }
  }

  # Filter for non-NA rows in the specified date variables
  data_filtered <- data[!is.na(data[[varm1]]) &
                          !is.na(data[[varj1]]) &
                          !is.na(data[[varm2]]) &
                          !is.na(data[[varj2]]), ]

  # Pull out values for date vars
  var1date <- (data_filtered[[varj1]] * 12) + data_filtered[[varm1]]
  var2date <- (data_filtered[[varj2]] * 12) + data_filtered[[varm2]]

  # Map comparison operators to logical actions
  comparison_map <- list(
    "smaller" = function() var1date < var2date,
    "smaller_equal" = function() var1date <= var2date,
    "larger" = function() var1date > var2date,
    "larger_equal" = function() var1date >= var2date,
    "equal" = function() var1date == var2date
  )

  if (!(comparison %in% names(comparison_map))) {
    stop("comparison argument must be 'smaller', 'smaller_equal', 'larger', 'larger_equal' or 'equal'")
  }

  # Identify case IDs where the test fails
  condition <- comparison_map[[comparison]]() # here, the list of functions is subsetted by the comparison argument and the subsetted function is then executed
  failed_cases <- data_filtered$caseid[!condition]

  if (length(failed_cases) > 0) {
    failed_sample <- utils::head(failed_cases, 5)  # Sample up to 5 case IDs
    stop(paste0("Error: There are observations where the first date does not meet the comparison (caseids: ", paste(failed_sample, collapse = ", "), ")"))
  } else {
    print(paste("Success: First date is always", comparison, "than second date"))
  }
}


#' Set specific values to NA
#'
#' @description This is used to quickly set NA values for specific missing codes in the NEPS data.
#' If only the data argument is supplied, the function will set specific values in all variables to NA.
#'
#' @param data A neps field-data dataframe.
#' @param vars A string vector with variable names where specific values will be replaced with NA.
#' @param values_to_replace A numerical vector with values that will be replaced with NA. By default most of the standard NEPS missings will be converted to NA.
#'
#' @export replace_values_with_na
replace_values_with_na <- function(data, vars = NULL, values_to_replace = c(seq(-99, -90), seq(-56, -51), seq(-29, -22))) {
  # Case 1: If input is a dataframe
  if (is.data.frame(data)) {
    if (is.null(vars)) {
      # Use function on all variables in the dataframe
      for (var in names(data)) {
        for (value in values_to_replace) {
          data[[var]][data[[var]] == value] <- NA
        }
      }
    }
    else {
      # Use function on selected variables in the dataframe
      for (var in vars) {
        for (value in values_to_replace) {
          data[[var]][data[[var]] == value] <- NA
        }
      }
    }

    # Case 2: If input is a vector
  } else if (is.vector(data)) {
    for (value in values_to_replace) {
      data[data == value] <- NA
    }

    # Case 3: If input is neither a dataframe nor a vector
  } else {
    stop("Input data must be either a dataframe or a vector.")
  }

  return(data)
}

#' Replace season codes in date variables with corresponding months
#'
#' @description This function will transform neps season codes to month values by subtracting 20 from each season code.
#' E.g. code 24 - "Spring" will be 4 - "April".
#'
#' @param data A neps field-data dataframe.
#' @param vars A string vector with variable names where season codes will be replaced with actual months.
#' @param values_to_replace Values that will be replaced with months. Only need to be edited in case of new season codes.
#'
#' @export replace_season_codes
replace_season_codes <- function(data, vars = NULL, values_to_replace=c(21, 24, 27, 30, 32)) {
  if (is.null(vars)) {
    for(var in names(data)) {
      for(value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value - 20
      }
    }
  } else {
    for(var in vars) {
      for(value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value - 20
      }
    }
  }
  return(data)
}


#' Expand episode data to monthly structure
#'
#' @description This function is typically used when we transform episode data into monthly data. It requires a duration variable and then replicates rows according to the duration variable.
#'
#' @param data A dataframe
#' @param duration A String with a variable name that will be used for expanding. Usually a numerical duration variable.
#' @export
expand <- function(data, duration) {
  # Check if the input data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # Check if duration is a string (character) and exists in the data frame
  if (!is.character(duration) || length(duration) != 1) {
    stop("The duration argument must be a string (character) and specify a single column name.")
  }

  if (!(duration %in% colnames(data))) {
    stop(paste("The column", duration, "does not exist in the data frame."))
  }

  # Validate that the duration column is non-negative and not NA
  if (any(data[[duration]] < 0) | any(is.na(data[[duration]]))) {
    stop("Please ensure the duration argument is a valid non-negative number and not NA.") # duration must be > 0 and not NA
  }

  # Expanding feature
  expanded_data <- data[rep(seq_len(nrow(data)), data[[duration]]), 1:ncol(data)]
  row.names(expanded_data) <- NULL  # Reset row names

  return(expanded_data)  # Return the expanded data
}

