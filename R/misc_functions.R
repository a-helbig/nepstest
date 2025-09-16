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



#' Test event or episode date variables
#' @description date range testing: supply 7 arguments when testing events, supply 9 arguments when testing episodes. After data argument, start with the two check dates, e.g. intmPRE, intjPRE, intm, intj. Then add more arguments, either an event date to test, e.g. fphm, fphj or an episode like ezstm, ezstj, ezendm, ezendj.
#'
#' @param data A neps field-data dataframe.
#' @param pm1 A String that holds the name of a monthly date variable which value will be used to test the date ranges of the target variable.
#' @param pj1 A String that holds the name of a yearly date variable which value will be used to test the date ranges of the target variable.
#' @param pm2 A String that holds the name of a monthly date variable which value will be used to test the date ranges of the target variable.
#' @param pj2 A String that holds the name of a yearly date variable which value will be used to test the date ranges of the target variable.
#' @param varm1 A String that holds the name of a monthly date variable which is usually the start date of the episode
#' @param varj1 A String that holds the name of a yearly date variable which is usually the start date of the episode
#' @param varm2 A String that holds the name of a monthly date variable which is usually the end date of the episode. This second date variable is only used when testing episode dates and not for event dates
#' @param varj2 A String that holds the name of a yearly date variable which is usually the end date of the episode This second date variable is only used when testing episode dates and not for event dates
#'
#' @returns either an error with an informative message about the type of error and the caseids involved or a success output print which wont stop the script
#'
#' @examples test_data <- data.frame(caseid = c(12393535), intmPRE = c(-97), intjPRE = c(2020),
#' ezstm = c(-97), ezstj = c(2024), ezendm = c(6), ezendj = c(-97), intm = c(12), intj = c(2024))
#' date_test_complex(test_data,"intmPRE","intjPRE","intm","intj","ezstm","ezstj","ezendm","ezendj")
#'
#' @export
date_test_complex <- function(data, pm1, pj1, pm2, pj2, varm1, varj1, varm2 = NULL, varj2 = NULL) {

  # Check if all specified variables exist in the dataframe
  var_names <- c(varm1, pm1, pj1, varj1, pm2, pj2, "caseid")  # Include caseid in the checks
  if (!is.null(varm2) && !is.null(varj2)) {
    var_names <- c(var_names, varm2, varj2)  # Add var2 variables only if they are not NULL
  }

  for (var in var_names) {
    if (!var %in% names(data)) {
      stop(paste("ERROR: Variable", var, "not found in the dataframe"))
    }
  }

  data <- replace_season_codes(data, c(pm1, pj1, pm2, pj2, varm1, varj1, varm2, varj2))

  # Filter for non-NA rows in the specified date variables
  data <- data[!is.na(data[[varm1]]) &
                 !is.na(data[[varj1]]) &
                 !is.na(data[[pm1]]) &
                 !is.na(data[[pj1]]) &
                 !is.na(data[[pm2]]) &
                 !is.na(data[[pj2]]), ]

  if (!is.null(varm2) && !is.null(varj2)) {
    # Include var2 checks only when not NULL and filter out NAs
    data <- data[!is.na(data[[varm2]]) & !is.na(data[[varj2]]), ]
  }

  # Set missing codes to NA
  data <- replace_values_with_na(data, vars=c(pm1, pj1, pm2, pj2, varm1, varj1, varm2, varj2))

  # Generate variables for the date test. First, we create date variables out of year and month variable. Second, we create a minimum variable for that date variable that gets january assigned as a minimum for the monthly level in case month is NA. Third, we create a maximum variable for the date variable that gets december assigned as a maximum for the monthly level in case month is NA. This procedure mirrors the process of how infas is supposed to deal with missings in month variables with regard to date checks in the ra-tags. If year variable has NA, all 3 generated variables will be NA and dropped in the checks below ("due to na.rm = T")
  data$p1 <- (data[[pj1]] * 12) + data[[pm1]]
  data$p1min <- ifelse(is.na(data[[pm1]]) & !is.na(data[[pj1]]), 1 + data[[pj1]] * 12, data$p1)
  data$p1max <- ifelse(is.na(data[[pm1]]) & !is.na(data[[pj1]]), 12 + data[[pj1]] * 12, data$p1)

  data$p2 <- (data[[pj2]] * 12) + data[[pm2]]
  data$p2min <- ifelse(is.na(data[[pm2]]) & !is.na(data[[pj2]]), 1 + data[[pj2]] * 12, data$p2)
  data$p2max <- ifelse(is.na(data[[pm2]]) & !is.na(data[[pj2]]), 12 + data[[pj2]] * 12, data$p2)

  data$var1 <- (data[[varj1]] * 12) + data[[varm1]]
  data$var1min <- ifelse(is.na(data[[varm1]]) & !is.na(data[[varj1]]), 1 + data[[varj1]] * 12, data$var1)
  data$var1max <- ifelse(is.na(data[[varm1]]) & !is.na(data[[varj1]]), 12 + data[[varj1]] * 12, data$var1)

  # Add var2 only if both varj2 and varm2 are not NULL
  if (!is.null(varm2) && !is.null(varj2)) {
    # Calculate var2 and its min/max versions using base R
    data$var2 <- (data[[varj2]] * 12) + data[[varm2]]

    # Calculate var2min
    data$var2min <- ifelse(is.na(data[[varm2]]) & !is.na(data[[varj2]]),
                           1 + data[[varj2]] * 12, data$var2)

    # Calculate var2max
    data$var2max <- ifelse(is.na(data[[varm2]]) & !is.na(data[[varj2]]),
                           12 + data[[varj2]] * 12, data$var2)
  }

  # Initialize a list to store failing caseids
  failing_cases_list <- list()

  # 1. Check: Internal consistency (p1 vs p2),
  # Applies to all checks: Exclude NA, so missings in year variables will be disregarded (following the test conventions)
  # Applies to all checks: Collect failing case IDs in a numeric vector and add it to the list, naming the vector based on the specific condition that caused the failure.
  if (any(data$p1min > data$p2max, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$p1min > data$p2max & !is.na(data$p1min) & !is.na(data$p2max)]
    failing_cases_list <- c(failing_cases_list, list("p1 > p2" = unique(failing_cases)))
  }

  # 2. Check: Internal consistency (var1 vs var2) - only if var2 is available
  if (!is.null(varm2) && !is.null(varj2) && any(data$var1min > data$var2max, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$var1min > data$var2max & !is.na(data$var1min) & !is.na(data$var2max)]
    failing_cases_list <- c(failing_cases_list, list("var1 > var2" = unique(failing_cases)))
  }

  # 3. Check: External consistency p1 vs var1
  if (any(data$p1min > data$var1max, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$p1min > data$var1max & !is.na(data$p1min) & !is.na(data$var1max)]
    failing_cases_list <- c(failing_cases_list, list("p1 > var1" = unique(failing_cases)))
  }

  # 4. Additional check for p1 vs var2 - only if var2 is available
  if (!is.null(varm2) && !is.null(varj2) && any(data$p1min > data$var2max, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$p1min > data$var2max & !is.na(data$p1min) & !is.na(data$var2max)]
    failing_cases_list <- c(failing_cases_list, list("p1 > var2" = unique(failing_cases)))
  }

  # 5. Check: External consistency p2 vs var2 - only if var2 is available
  if (!is.null(varm2) && !is.null(varj2) && any(data$p2max < data$var2min, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$p2max < data$var2min & !is.na(data$p2max) & !is.na(data$var2min)]
    failing_cases_list <- c(failing_cases_list, list("p2 < var2" = unique(failing_cases)))
  }

  # 6. Check: External consistency p2 vs var1
  if (any(data$p2max < data$var1min, na.rm = TRUE)) {
    failing_cases <- data$caseid[data$p2max < data$var1min & !is.na(data$p2max) & !is.na(data$var1min)]
    failing_cases_list <- c(failing_cases_list, list("p2 < var1" = unique(failing_cases)))
  }

  # After all checks, report any failing caseids
  if (length(failing_cases_list) > 0) {
    error_message <- "The following cases failed the checks:\n"
    for (condition in names(failing_cases_list)) {
      error_message <- paste0(error_message, condition, ": ",
                              paste(unique(failing_cases_list[[condition]]), collapse = ", "), "\n")
    }
    stop(error_message)
  } else {
    print("All checks passed successfully.")
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
replace_season_codes <- function(data, vars, values_to_replace = c(21, 24, 27, 30, 32)) {

  # Throw an error if data is not a dataframe
  if (!is.data.frame(data)) {
    stop("Error: The 'data' argument must be a dataframe. Please provide a valid dataframe.")
  }

  # Throw an error if vars is missing
  if (missing(vars) || is.null(vars)) {
    stop("Error: The 'vars' argument is missing or NULL. Please provide a vector containing the names of the monthly date variables where season codes should be replaced with corresponding months.")
  }

  # Throw an error if values_to_replace is not a numeric vector
  if (!is.numeric(values_to_replace) || length(values_to_replace) == 0) {
    stop("Error: The 'values_to_replace' argument must be a non-empty numeric vector. Please provide valid numeric values for replacement.")
  }

  # Process the data based on the vars argument
  if (length(vars) == 0) {
    for (var in names(data)) {
      for (value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value - 20
      }
    }
  } else {
    for (var in vars) {
      for (value in values_to_replace) {
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

