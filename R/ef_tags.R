
#' Tests ef-tags (entry filters)
#'
#' @description
#' This function is designed for testing ef-tags, i.e. entry filters that state under which condition an item is asked.
#' An item with an ef-tag must be answered (not NA) in every row where its entry condition holds, and it must be NA in every
#' row where the condition does not hold. The function checks both directions:
#'
#' * Test 1 (asked): `test_var` is not NA where `condition` is TRUE. Fails reveal underfiltering, i.e. the item was not asked
#'   although it should have been (or the interview was broken off before the item).
#' * Test 2 (not asked): `test_var` is NA where `condition` is FALSE. Fails reveal overfiltering, i.e. the item was asked
#'   although it should not have been.
#'
#' Values like -97 or -98 count as answered, only NA (and empty strings "" in character variables) count as not asked.
#'
#' @details
#' `condition` is an R expression that is evaluated inside `data`, so it can combine several variables freely. To translate
#' the PV syntax, use:
#'
#' * `x = 1, 2` becomes `x %in% c(1, 2)`
#' * `x <> 2` becomes `!x %in% 2` (this is also TRUE if x is missing, like in the CATI logic)
#' * `x <> .` becomes `!is.na(x)` and `x = .` becomes `is.na(x)`
#' * `&` and `OR` become `&` and `|`. Several `if` lines of one ef-tag are alternatives and are combined with `|`.
#'
#' Prefer `%in%` over `==` and `!=`: comparisons with `==`, `!=`, `<` or `>` return NA if a variable is missing. Rows where
#' the condition is NA are treated as "condition not met", and the function prints a note how many rows that affected.
#'
#' The ef-tag of an enclosing block (e.g. "Beginn Block 2") applies to all items in the block. Either add it to `condition`
#' with `&`, or filter `data` to the block condition beforehand, as you would with the other test functions.
#'
#' With `use_duration = TRUE` (default), a value in Test 2 is tolerated if a corresponding duration variable
#' (`<var>duration`, `scr_<var>duration` or `src_<var>duration`) exists and is 0 or NA, because this means the item was
#' not shown, like in [af_test_complex()]. This matters for items that are filled by an autoif without being asked,
#' e.g. an end date that is set to the interview date if the episode is ongoing. Month and year variables of a date
#' (e.g. `alendm`, `alendj`) have no duration variable of their own, so the duration variable of the date is used
#' instead (e.g. `scr_alenddatduration`). The output names the duration variable that was used. Set
#' `use_duration = FALSE` for a strict Test 2 that ignores duration variables.
#'
#' @param data A neps field-data dataframe.
#' @param test_var A string with the name of the item that carries the ef-tag. Can also be a character vector of several
#'   items that share the same entry condition.
#' @param condition The entry condition as an unquoted R expression, e.g. `h_panel == 1 & !alintrop %in% 2`.
#'   Use `TRUE` for items that are always asked. Objects from your script (e.g. a vector of codes) can be used too.
#'   A single string with the expression is accepted as well, e.g. `"h_panel == 1"`, and so is a variable that holds
#'   a string or a quoted expression, e.g. `cond <- quote(h_panel == 1)`, which is handy if several items share a condition.
#' @param check Which direction to test: `"both"` (default), `"asked"` (only Test 1) or `"not_asked"` (only Test 2).
#'   `"not_asked"` can be useful if many interviews were broken off and Test 1 is therefore not informative.
#' @param use_duration Logical, if TRUE (default) Test 2 tolerates a value when the corresponding duration variable is 0 or
#'   NA. If FALSE, Test 2 fails for every value where the condition is not met. Has no effect for items without a duration
#'   variable.
#' @param print_filter_vars Optional character vector. Additional variable names to be included and prioritized in the
#'   printed output when errors are detected. Variables used in `condition` are always printed next to `test_var`.
#'
#' @return Invisibly returns TRUE if all tests pass, otherwise stops with an error after printing the problematic cases.
#'
#' @examples
#' \dontrun{
#' # aliz: --ef: if (h_panel = 1)
#' ef_test(T_27AL, "aliz", h_panel == 1)
#'
#' # aliz2: --ef: if (h_panel = 0 & h_dauertan = 2)
#' ef_test(T_27AL, "aliz2", h_panel == 0 & h_dauertan == 2)
#'
#' # almb: --ef: if (almod1 = 1, 2, 4), within Block 2 with --ef: if (alintrop <> 2)
#' ef_test(T_27AL, "almb", !alintrop %in% 2 & almod1 %in% c(1, 2, 4))
#'
#' # alenddat: two ef lines are alternatives: if (h_panel = 0) / if (aliz = 2)
#' ef_test(T_27AL, c("alendm", "alendj"), !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))
#'
#' # only check for overfiltering
#' ef_test(T_27AL, "alge", alme %in% 1, check = "not_asked")
#'
#' # item that is always asked (--ef: alle)
#' ef_test(T_27AL, "alnr", TRUE)
#'
#' # codes defined in the script can be used in the condition
#' panel_modes <- c(2, 3)
#' ef_test(T_27AL, "alintro12", almod1 %in% panel_modes)
#'
#' # strict Test 2 without the duration rule
#' ef_test(T_27AL, "algb", almb %in% 1, use_duration = FALSE)
#' }
#'
#' @export ef_test
ef_test <- function(data, test_var, condition, check = c("both", "asked", "not_asked"),
                    use_duration = TRUE, print_filter_vars = NULL) {
  data_name <- deparse(substitute(data))
  check <- match.arg(check)
  if (!is.logical(use_duration) || length(use_duration) != 1 || is.na(use_duration)) {
    stop("ERROR: use_duration must be TRUE or FALSE.")
  }

  # Capture the condition as an expression. Accept an unquoted expression, a string, or a variable holding a string.
  cond_expr <- substitute(condition)
  if (is.symbol(cond_expr) && !as.character(cond_expr) %in% names(data)) {
    cond_value <- tryCatch(eval(cond_expr, parent.frame()), error = function(e) NULL)
    if (is.character(cond_value) || is.call(cond_value)) cond_expr <- cond_value
  }
  if (is.character(cond_expr)) {
    if (length(cond_expr) != 1) {
      stop("ERROR: condition must be a single expression or a single string.")
    }
    cond_expr <- str2lang(cond_expr)
  }
  cond_label <- paste(deparse(cond_expr), collapse = " ")

  # Check variable presence
  if (!is.character(test_var) || length(test_var) == 0) {
    stop("ERROR: test_var must be a character vector with at least one variable name.")
  }
  missing_test_vars <- test_var[!test_var %in% names(data)]
  if (length(missing_test_vars) > 0) {
    stop(paste("ERROR: test_var", paste(missing_test_vars, collapse = ", "), "not found in the dataframe", data_name))
  }
  # Names in the condition are columns of data. Names that are not columns may refer to objects in the calling
  # environment (e.g. a vector of codes defined in the script); anything else is a typo and stops the function.
  caller_env <- parent.frame()
  all_cond_names <- all.vars(cond_expr)
  cond_vars <- all_cond_names[all_cond_names %in% names(data)]
  missing_cond_vars <- all_cond_names[!all_cond_names %in% names(data) &
                                        !vapply(all_cond_names, exists, logical(1), envir = caller_env)]
  if (length(missing_cond_vars) > 0) {
    stop(paste("ERROR: Variable(s) used in condition not found in the dataframe", data_name, ":",
               paste(missing_cond_vars, collapse = ", ")))
  }
  if (any(test_var %in% cond_vars)) {
    stop("ERROR: test_var must not be used in its own condition.")
  }

  # Evaluate the condition inside the data
  cond <- eval(cond_expr, as.list(data), caller_env)
  if (is.logical(cond) && length(cond) == 1) {
    cond <- rep(cond, nrow(data)) # e.g. TRUE for items that are always asked
  }
  if (!is.logical(cond) || length(cond) != nrow(data)) {
    stop("ERROR: condition must evaluate to a logical vector with one value per row of the dataframe.")
  }
  cond <- as.vector(cond)
  n_cond_na <- sum(is.na(cond))
  if (n_cond_na > 0) {
    message("NOTE: The condition evaluates to NA in ", n_cond_na, " row(s), these are treated as 'condition not met'. ",
            "Use %in% or is.na() in the condition to handle missings explicitly.")
    cond[is.na(cond)] <- FALSE
  }

  n_met <- sum(cond)
  message("Condition '", cond_label, "' is met in ", n_met, " of ", nrow(data), " row(s).")
  if (n_met == 0) {
    message("NOTE: The condition is never met, so Test 1 cannot detect anything. Please check the condition.")
  }

  id_var <- if ("caseid" %in% names(data)) "caseid" else NULL
  any_fail <- FALSE

  for (var in test_var) {
    values <- replace_empty_with_na_ef(data[[var]])

    ### Test 1: asked where the condition holds
    if (check %in% c("both", "asked")) {
      fail1 <- cond & is.na(values)
      if (any(fail1)) {
        any_fail <- TRUE
        message("Test 1 (asked): ERROR - '", var, "' is NA in ", sum(fail1),
                " row(s) where the condition is met (item not asked or interview broken off).",
                ef_ids_text(data, id_var, fail1))
        print(ef_print_data(data, fail1, id_var, var, cond_vars, print_filter_vars))
      } else {
        message("Test 1 (asked): SUCCESS - '", var, "' is not NA in all rows where the condition is met.")
      }
    }

    ### Test 2: not asked where the condition does not hold
    if (check %in% c("both", "not_asked")) {
      fail2 <- !cond & !is.na(values)
      duration_var <- if (use_duration) find_duration_var(var, names(data)) else NULL
      if (!is.null(duration_var)) {
        duration <- data[[duration_var]]
        fail2 <- fail2 & !is.na(duration) & duration != 0
        message("Test 2 (not asked): values are tolerated where '", duration_var, "' is 0 or NA (item not shown).")
      }
      if (any(fail2)) {
        any_fail <- TRUE
        message("Test 2 (not asked): ERROR - '", var, "' is not NA in ", sum(fail2),
                " row(s) where the condition is not met (item asked although filtered out).",
                ef_ids_text(data, id_var, fail2))
        print(ef_print_data(data, fail2, id_var, var, c(cond_vars, duration_var), print_filter_vars))
      } else {
        message("Test 2 (not asked): SUCCESS - '", var, "' is NA in all rows where the condition is not met.")
      }
    }
  }

  if (any_fail) {
    stop("ef_test failed. Please inspect the above rows.")
  }

  invisible(TRUE)
}

# Helper to replace "" with NA in character vectors
replace_empty_with_na_ef <- function(x) {
  if (is.character(x)) {
    x[x == ""] <- NA
  }
  x
}

# Find a duration variable that belongs to var, preferring "<var>duration" over the scr_/src_ variants.
# Month and year variables of a date (e.g. alendm, alendj) have no duration variable of their own, so the duration
# variable of the date (e.g. scr_alenddatduration) is used for them.
find_duration_var <- function(var, var_names) {
  duration_candidates <- function(v) c(paste0(v, "duration"), paste0("scr_", v, "duration"), paste0("src_", v, "duration"))
  candidates <- duration_candidates(var)
  if (grepl("[mjy]$", var)) {
    candidates <- c(candidates, duration_candidates(paste0(sub("[mjy]$", "", var), "dat")))
  }
  found <- candidates[candidates %in% var_names]
  if (length(found) == 0) NULL else found[1]
}

# Text listing the problematic caseids, if a caseid variable exists
ef_ids_text <- function(data, id_var, fail_idx) {
  if (is.null(id_var)) return("")
  paste0(" Problematic caseid(s): ", paste(unique(data[[id_var]][fail_idx]), collapse = ", "))
}

# Rows to print, with caseid, test_var, condition variables and print_filter_vars first
ef_print_data <- function(data, fail_idx, id_var, var, cond_vars, print_filter_vars) {
  print_data <- data[fail_idx, , drop = FALSE]
  valid_filter_vars <- print_filter_vars[print_filter_vars %in% names(print_data)]
  cols_order <- unique(c(id_var, var, cond_vars, valid_filter_vars))
  cols_order <- c(cols_order, setdiff(names(print_data), cols_order))
  print_data[, cols_order, drop = FALSE]
}
