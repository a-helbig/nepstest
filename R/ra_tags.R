#' Tests date ranges (ra-tags) of event or episode dates
#'
#' @description
#' Tests whether dates lie inside the range that the ra-tag of a date item allows. Typically the range is "between the
#' date of the previous interview and the date of the current interview". The function can test
#'
#' * an **event**: one date (month and year), e.g. the date of a birth, with 7 arguments, or
#' * an **episode**: a start and an end date, e.g. an employment episode, with 9 arguments.
#'
#' All variables are given as strings. The arguments are positional and always come in month/year pairs: first the two
#' dates that limit the range (`pm1`/`pj1` = lower limit, `pm2`/`pj2` = upper limit), then the date(s) to test
#' (`varm1`/`varj1` = start or event date, `varm2`/`varj2` = end date of an episode).
#'
#' @details
#' Before the comparison, season codes in the month variables (21, 24, 27, 30, 32) are turned into months (1, 4, 7,
#' 10, 12) and NEPS missing codes (-99 to -90, -56 to -51, -29 to -22) are set to NA. Rows in which one of the date
#' variables is NA (i.e. the item was not asked) are dropped.
#'
#' If only the month is missing (e.g. -97 "verweigert" but a valid year), the date is treated as "some time in that
#' year": January for the earliest and December for the latest possible date. This is the same rule the survey
#' institute uses for the ra-tag checks in the CATI. If the year is missing, the date is not checked.
#'
#' The function runs up to six checks and reports every failed check with the problematic caseids:
#'
#' * `p1 > p2`: the lower limit lies after the upper limit (should not happen, points to a problem with the preloads).
#' * `var1 > var2`: the episode ends before it starts (episodes only).
#' * `p1 > var1` / `p1 > var2`: the start or end date lies before the lower limit.
#' * `p2 < var1` / `p2 < var2`: the start or end date lies after the upper limit.
#'
#' The data needs a `caseid` variable.
#'
#' @param data A neps field-data dataframe.
#' @param pm1,pj1 Month and year variable of the **lower limit** of the range, e.g. `"intmPRE"`, `"intjPRE"` (date of
#'   the previous interview).
#' @param pm2,pj2 Month and year variable of the **upper limit** of the range, e.g. `"intm"`, `"intj"` (date of the
#'   current interview).
#' @param varm1,varj1 Month and year variable of the date to test. For episodes this is the start date, e.g.
#'   `"ezstm"`, `"ezstj"`.
#' @param varm2,varj2 Month and year variable of the end date of an episode, e.g. `"ezendm"`, `"ezendj"`. Leave out
#'   for events.
#'
#' @return Prints a success message if all checks pass. Otherwise stops with an error that lists the failed checks and
#'   the caseids involved.
#'
#' @family test functions
#'
#' @examples
#' test_data <- data.frame(
#'   caseid = c(1, 2),
#'   intmPRE = c(12, 3), intjPRE = c(2023, 2023), # previous interview
#'   intm = c(10, 11), intj = c(2024, 2024),      # current interview
#'   ezstm = c(1, 21), ezstj = c(2024, 2024),     # 21 = Jahresanfang/Winter
#'   ezendm = c(6, -97), ezendj = c(2024, 2024)   # -97 = verweigert
#' )
#'
#' # episode: start and end date must lie between the two interviews
#' date_test_complex(test_data, "intmPRE", "intjPRE", "intm", "intj",
#'                   "ezstm", "ezstj", "ezendm", "ezendj")
#'
#' # event: only one date to test
#' date_test_complex(test_data, "intmPRE", "intjPRE", "intm", "intj", "ezstm", "ezstj")
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
    error_message <- "ERROR: The following cases failed the checks:\n"
    for (condition in names(failing_cases_list)) {
      error_message <- paste0(error_message, condition, ": ",
                              paste(unique(failing_cases_list[[condition]]), collapse = ", "), "\n")
    }
    stop(error_message)
  } else {
    print("SUCCESS:: All checks passed successfully.")
  }
}

#' Set NEPS missing codes to NA
#'
#' @description
#' Internal helper of [date_test_complex()]. Sets the standard NEPS missing codes (by default -99 to -90, -56 to -51
#' and -29 to -22) to NA, either in the variables given in `vars` or, if `vars` is NULL, in all variables. Also works
#' on a single vector. For your own scripts, use the corresponding function of the nepstools package.
#'
#' @param data A neps field-data dataframe or a vector.
#' @param vars Character vector of variable names. If NULL, all variables are processed.
#' @param values_to_replace Numeric vector of codes that are set to NA.
#'
#' @return `data` with the codes set to NA.
#' @keywords internal
#' @noRd
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

#' Replace season codes in month variables with months
#'
#' @description
#' Internal helper of [date_test_complex()]. Turns NEPS season codes into months by subtracting 20, e.g. code 24
#' becomes 4 (April). For your own scripts, use the corresponding function of the nepstools package.
#'
#' @param data A neps field-data dataframe.
#' @param vars Character vector with the month variables.
#' @param values_to_replace The season codes (21, 24, 27, 30, 32).
#'
#' @return `data` with the season codes replaced.
#' @keywords internal
#' @noRd
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
