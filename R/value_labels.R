#' Expected value labels from a PV
#'
#' @description
#' Derives from the we-tags of a PV which codes and value labels each variable should have in the field data. Grouped
#' items (dates, multiple choice items, lists) are split into one set of codes per variable, like in [pv_var_labels()].
#'
#' @details
#' The lines of a we-tag are read as follows:
#'
#' * `1: Arbeiter/in` gives code 1 with the label "Arbeiter/in".
#' * Conditions in front of a code are removed: `if (h_etauto = 0) 5: Mitarbeiter` gives code 5.
#' * `BUTTONS: verweigert (-97), ...` gives code -97 with the label "verweigert", and so on. Conditions in the buttons
#'   (`[if h_panel = 0] bis heute (-5)`) are removed from the label.
#' * Hints for the interviewer in `<< >>`, `OFFEN:` lines and date fields (`Monat ____ Jahr _____`) give no codes.
#' * Multiple choice items: the lines in square brackets (`[0: nicht genannt]`, `[1: genannt]`) are the codes of every
#'   variable of the item. The lines for the single variables (`ts23525 1: Sonderzahlungen ...`) give no codes.
#' * Dates: the month variable gets the codes of the buttons and the season codes from the in-tag
#'   (`21=Jahresanfang/Winter`, ...). The other parts of the date only get the codes of the buttons.
#' * A list in square brackets (e.g. `[Gemeindeliste]`) means that the codes come from a list that is not
#'   in the PV. The name of the list is kept in the column `list`, the buttons of the item are read as usual.
#' * If the same code appears several times with different texts (e.g. under different conditions), every text is accepted.
#'
#' @param pv The result of [read_pv()], or a path (or several paths) to PV docx files.
#'
#' @return A data frame with one row per variable and code and the columns `var`, `code`, `label`, `list` (name of a
#'   list like `[Gemeindeliste]` the codes come from, otherwise NA), `item`, `va` and `pv`. Variables of the PV without
#'   any code (e.g. open questions without buttons) have one row with `code` NA.
#'
#' @examples
#' \dontrun{
#' values <- nepstest:::pv_value_labels("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
#' values[values$var == "etbs", ]
#' }
#'
#' @seealso [value_label_test()], which uses this function. Call it yourself (`nepstest:::pv_value_labels()`) to
#'   see which codes a variable is expected to have.
#' @keywords internal
pv_value_labels <- function(pv) {
  pv <- as_pv(pv)
  items <- pv[!is.na(pv$va) & nzchar(trimws(pv$va)), , drop = FALSE]
  if (nrow(items) == 0) {
    stop("ERROR: the PV contains no items with a va-tag.")
  }
  col <- function(name) if (name %in% names(items)) items[[name]] else rep(NA_character_, nrow(items))
  vb <- col("vb")
  we <- col("we")
  instr <- col("in")

  rows <- lapply(seq_len(nrow(items)), function(i) {
    out <- expand_item_values(items$va[i], vb[i], we[i], instr[i])
    out$item <- rep(items$item[i], nrow(out))
    out$va <- rep(items$va[i], nrow(out))
    out$pv <- rep(items$pv[i], nrow(out))
    out$list <- rep(parse_we_tag(we[i])$list, nrow(out))
    out
  })
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out[, c("var", "code", "label")]), c("var", "code", "label", "list", "item", "va", "pv")]
  rownames(out) <- NULL
  out
}

#' Tests value labels against the PV
#'
#' @description
#' Compares the value labels (codes and their labels) in a field-data dataframe with the codes of the we-tags in the PV
#' (see [pv_value_labels()]). One call checks all variables of a module.
#'
#' @details
#' Variables are matched to the PV like in [label_test()]: by name, loop variables with a suffix like `_1` by the name
#' without the suffix, and variables matching `ignore` are skipped. The value labels in the data are the attribute
#' `labels` as set by [haven::read_dta()], a named vector with the codes as values and the labels as names.
#'
#' For every variable and code the function checks:
#'
#' * the code is in the PV and in the data, and the labels are identical (apart from blanks): fine.
#' * the labels differ only in brackets, slashes, hyphens, quotes, colons, commas or upper/lower case: fine, counted as a
#'   formatting difference. Use `show_format = TRUE` to list them.
#' * the labels differ otherwise: ERROR.
#' * the code is in the PV but has no label in the data: ERROR.
#' * the code has a label in the data but is not in the PV: ERROR.
#' * the label in the PV is only a fill-in like `<h_etarta1>`: only the code is checked, the text is listed as a NOTE.
#' * the item takes its codes from a list that is not in the PV (e.g. a country list): the codes in the data that are not
#'   in the PV are not checked and counted in a NOTE. The buttons of the item are checked as usual.
#'
#' Character variables (open answers) cannot carry value labels in Stata and are skipped. The function prints all
#' differences per variable and stops at the end, like the other test functions.
#'
#' @param data A neps field-data dataframe, read with [haven::read_dta()].
#' @param pv The result of [read_pv()], or a path (or several paths) to PV docx files.
#' @param ignore Character vector of regular expressions. Variables in the data whose names match are not checked.
#'   Default: duration variables and variables ending in `_e`.
#' @param show_format Logical, if TRUE the labels that differ only in formatting are printed as well.
#' @param stop_on_fail Logical, if TRUE (default) the function stops with an error if a difference is found. If FALSE it
#'   only prints the results and returns the result table.
#'
#' @return Invisibly a data frame with one row per variable and code and the columns `var`, `pv_var`, `code`,
#'   `data_label`, `pv_label`, `status` ("ok", "format", "fill_in", "list", "mismatch", "missing_in_data", "not_in_pv") and
#'   `item`.
#'   If differences are found and `stop_on_fail = TRUE`, the function stops with an error after printing them.
#'
#' @examples
#' \dontrun{
#' T_26ET <- haven::read_dta("26ET.dta")
#' pv_26ET <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
#' value_label_test(T_26ET, pv_26ET)
#'
#' res <- value_label_test(T_26ET, pv_26ET, stop_on_fail = FALSE)
#' res[res$status != "ok", ]
#' }
#'
#' @family test functions
#' @export value_label_test
value_label_test <- function(data, pv, ignore = c("duration$", "_e$"), show_format = FALSE, stop_on_fail = TRUE) {
  data_name <- deparse(substitute(data))
  if (!is.data.frame(data)) {
    stop("ERROR: data must be a dataframe.")
  }
  for (arg in c("show_format", "stop_on_fail")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1 || is.na(value)) stop(paste0("ERROR: ", arg, " must be TRUE or FALSE."))
  }

  expected <- if (is.data.frame(pv) && all(c("var", "code", "label") %in% names(pv))) pv else pv_value_labels(pv)
  if (!"item" %in% names(expected)) expected$item <- NA_character_
  if (!"list" %in% names(expected)) expected$list <- NA_character_

  data_vars <- names(data)
  if (length(ignore) > 0) {
    ignored <- Reduce(`|`, lapply(ignore, function(p) grepl(p, data_vars)))
    data_vars <- data_vars[!ignored]
  }
  pv_vars <- unique(expected$var)
  base_vars <- sub("_[0-9]+$", "", data_vars)
  pv_var <- ifelse(data_vars %in% pv_vars, data_vars, ifelse(base_vars %in% pv_vars, base_vars, NA_character_))
  is_char <- vapply(data_vars, function(v) is.character(data[[v]]), logical(1), USE.NAMES = FALSE)
  check <- !is.na(pv_var) & !is_char

  rows <- lapply(which(check), function(i) {
    v <- data_vars[i]
    exp_v <- expected[expected$var == pv_var[i] & !is.na(expected$code), , drop = FALSE]
    list_name <- expected$list[expected$var == pv_var[i]][1]
    compare_value_labels(v, pv_var[i], attr(data[[v]], "labels", exact = TRUE), exp_v, list_name)
  })
  result <- do.call(rbind, c(list(empty_value_result()), rows))
  rownames(result) <- NULL

  message("Comparing the value labels of ", sum(check), " variable(s) in ", data_name, " with the PV.")
  if (any(!is.na(pv_var) & is_char)) {
    message("NOTE: ", sum(!is.na(pv_var) & is_char), " character variable(s) were skipped (open answers have no value labels).")
  }

  fail_status <- c("mismatch", "missing_in_data", "not_in_pv")
  fails <- result[result$status %in% fail_status, , drop = FALSE]
  if (nrow(fails) > 0) {
    message("ERROR - value labels of ", length(unique(fails$var)), " variable(s) differ from the PV:")
    print_value_rows(fails)
  } else {
    message("SUCCESS - no value label differs from the PV.")
  }

  from_list <- result[result$status == "list", , drop = FALSE]
  if (nrow(from_list) > 0) {
    message("NOTE: codes from a list that is not in the PV were not checked:")
    for (v in unique(from_list$var)) {
      message("  ", v, ": ", sum(from_list$var == v), " code(s) from ", from_list$pv_label[from_list$var == v][1])
    }
  }

  fill <- result[result$status == "fill_in", , drop = FALSE]
  if (nrow(fill) > 0) {
    message("NOTE: ", nrow(fill), " code(s) have only a fill-in as label in the PV (e.g. <h_etarta1>), only the code was ",
            "checked: ", paste(unique(fill$var), collapse = ", "))
  }

  n_format <- sum(result$status == "format")
  if (n_format > 0) {
    message("NOTE: ", n_format, " value label(s) differ only in brackets, slashes, hyphens, punctuation or case.",
            if (!show_format) " Use show_format = TRUE to list them." else "")
    if (show_format) print_value_rows(result[result$status == "format", , drop = FALSE])
  }

  if (nrow(fails) > 0 && stop_on_fail) {
    stop("value_label_test failed. Please inspect the above value labels.")
  }
  invisible(result)
}

# Empty result table with the right columns
empty_value_result <- function() {
  data.frame(var = character(0), pv_var = character(0), code = numeric(0), data_label = character(0),
             pv_label = character(0), status = character(0), item = character(0), stringsAsFactors = FALSE)
}

# Compare the value labels of one data variable with the expected codes of its PV variable
compare_value_labels <- function(var, pv_var, data_labels, expected, list_name = NA_character_) {
  data_codes <- if (is.null(data_labels)) numeric(0) else as.numeric(unname(data_labels))
  data_texts <- if (is.null(data_labels)) character(0) else names(data_labels)
  exp_codes <- unique(expected$code)
  item <- if (nrow(expected) > 0) expected$item[1] else NA_character_
  codes <- sort(unique(c(exp_codes, data_codes)))
  if (length(codes) == 0) return(empty_value_result())

  rows <- lapply(codes, function(code) {
    pv_texts <- expected$label[expected$code == code]
    data_text <- if (code %in% data_codes) data_texts[match(code, data_codes)] else NA_character_
    if (length(pv_texts) == 0 && !is.na(list_name)) {
      status <- "list"
      pv_text <- list_name
    } else if (length(pv_texts) == 0) {
      status <- "not_in_pv"
      pv_text <- NA_character_
    } else if (is.na(data_text)) {
      status <- "missing_in_data"
      pv_text <- pv_texts[1]
    } else if (all(grepl("^<[^<>]+>$", pv_texts))) {
      status <- "fill_in"
      pv_text <- pv_texts[1]
    } else {
      st <- compare_labels(rep(data_text, length(pv_texts)), pv_texts)
      best <- if ("ok" %in% st) which(st == "ok")[1] else if ("format" %in% st) which(st == "format")[1] else 1
      status <- if (st[best] %in% c("ok", "format")) st[best] else "mismatch"
      pv_text <- if (status == "mismatch") paste(unique(pv_texts), collapse = " | ") else pv_texts[best]
    }
    data.frame(var = var, pv_var = pv_var, code = code, data_label = data_text, pv_label = pv_text,
               status = status, item = item, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Print differences grouped by variable
print_value_rows <- function(rows) {
  for (v in unique(rows$var)) {
    r <- rows[rows$var == v, , drop = FALSE]
    item <- if (!is.na(r$item[1])) paste0("  (item ", r$item[1], ")") else ""
    message("  ", v, item)
    for (i in seq_len(nrow(r))) {
      code <- format(r$code[i])
      text <- switch(r$status[i],
        missing_in_data = paste0("missing in data (PV: \"", r$pv_label[i], "\")"),
        not_in_pv = paste0("not in PV (data: \"", r$data_label[i], "\")"),
        paste0("data: \"", r$data_label[i], "\"  PV: \"", r$pv_label[i], "\"")
      )
      message("    ", formatC(code, width = 4), ": ", text)
    }
  }
}

# Expected codes for the variable(s) of one item. Returns a data frame with var, code, label (code NA = no codes).
expand_item_values <- function(va, vb, we, instr) {
  va_clean <- clean_va(va)
  m <- regmatches(va_clean, regexec("^([^ (]+)\\s*\\((.*)\\)$", va_clean))[[1]]
  vars <- if (length(m) == 0) va_clean else {
    v <- trimws(strsplit(m[3], "[/,]")[[1]])
    v[nzchar(v)]
  }
  parsed <- parse_we_tag(we, vars)
  make <- function(var, codes) {
    if (nrow(codes) == 0) {
      return(data.frame(var = var, code = NA_real_, label = NA_character_, stringsAsFactors = FALSE))
    }
    data.frame(var = var, code = codes$code, label = codes$label, stringsAsFactors = FALSE)
  }

  if (length(vars) == 1) {
    return(make(vars, rbind(parsed$codes, parsed$buttons)))
  }

  # Multiple choice: the codes in square brackets apply to every variable
  if (nrow(parsed$mf_codes) > 0 || parsed$has_var_lines) {
    return(do.call(rbind, lapply(vars, make, codes = parsed$mf_codes)))
  }

  # Dates: the month part gets the season codes from the in-tag
  parts <- NULL
  vb_clean <- clean_vb(vb)
  if (!is.na(vb_clean)) {
    group <- find_vb_list(vb_clean, length(vars))
    if (!is.null(group) && length(group$parts) == length(vars)) parts <- group$parts
  }
  if (!is.null(parts) && nrow(parsed$codes) == 0 && any(parts %in% c("Monat", "Jahr", "Tag"))) {
    seasons <- parse_season_codes(instr)
    return(do.call(rbind, lapply(seq_along(vars), function(k) {
      codes <- parsed$buttons
      if (parts[k] == "Monat") codes <- rbind(codes, seasons)
      make(vars[k], codes)
    })))
  }

  # Other lists (e.g. h_etzeit1, h_etzeit2, ...): the same codes for every variable
  do.call(rbind, lapply(vars, make, codes = rbind(parsed$codes, parsed$buttons)))
}

# Split a we-tag into plain codes, codes of the buttons and codes in square brackets (multiple choice).
# Lines that start with one of the item's variables (e.g. "ts23525 1: ...") mark a multiple choice item.
parse_we_tag <- function(we, vars = character(0)) {
  none <- data.frame(code = numeric(0), label = character(0), stringsAsFactors = FALSE)
  out <- list(codes = none, buttons = none, mf_codes = none, has_var_lines = FALSE, list = NA_character_)
  if (is.na(we)) return(out)
  lines <- squish(strsplit(we, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  add <- function(df, code, label) {
    label <- gsub("\\[\\s*if\\b[^]]*\\]|\\(\\s*if\\b[^)]*\\)", "", label, perl = TRUE)
    rbind(df, data.frame(code = as.numeric(code), label = squish(label), stringsAsFactors = FALSE))
  }

  for (line in lines) {
    if (grepl("^<<", line) || grepl("OFFEN", line)) next
    if (grepl("^BUTTONS", line)) {
      hits <- regmatches(line, gregexpr("[^:,()]+\\(\\s*-?[0-9]+\\s*\\)", line))[[1]]
      for (h in hits) {
        out$buttons <- add(out$buttons, sub("^.*\\(\\s*(-?[0-9]+)\\s*\\)$", "\\1", h), sub("\\(.*$", "", h))
      }
      next
    }
    if (grepl("^[\\[{]|[\\]}]$", line, perl = TRUE)) {
      inner <- gsub("^[\\[{]\\s*|\\s*[\\]}]$", "", line, perl = TRUE)
      if (grepl("liste", inner, ignore.case = TRUE) && !grepl(":", inner, fixed = TRUE)) {
        out$list <- paste0("[", inner, "]")
      } else if (grepl("^-?[0-9]+\\s*:", inner)) {
        out$mf_codes <- add(out$mf_codes, sub("^(-?[0-9]+)\\s*:.*$", "\\1", inner), sub("^-?[0-9]+\\s*:\\s*", "", inner))
      }
      next
    }
    line <- strip_we_condition(line)
    var_name <- sub("^([A-Za-z][A-Za-z0-9_]*)(\\s+-?[0-9]+)?\\s*:.*$", "\\1", line)
    if (var_name != line && var_name %in% vars) {
      out$has_var_lines <- TRUE
      next
    }
    if (grepl("^-?[0-9]+\\s*:", line)) {
      label <- sub("^-?[0-9]+\\s*:\\s*", "", line)
      label <- squish(gsub("<<.*?>>", "", label, perl = TRUE))
      out$codes <- add(out$codes, sub("^(-?[0-9]+)\\s*:.*$", "\\1", line), label)
    }
  }
  out
}

# Remove a leading condition from a we-line: "if (a = 1) 5: x", "if ((a) & b) 5: x", "(if a = 1) x", "if <a> 5: x"
strip_we_condition <- function(line) {
  if (grepl("^if\\s*<", line)) return(squish(sub("^if\\s*<[^>]*>", "", line)))
  if (!grepl("^\\(?\\s*if\\b", line)) return(line)
  chars <- strsplit(line, "")[[1]]
  start <- which(chars == "(")[1]
  if (is.na(start)) return(line)
  depth <- 0
  for (k in start:length(chars)) {
    if (chars[k] == "(") depth <- depth + 1
    if (chars[k] == ")") depth <- depth - 1
    if (depth == 0) return(squish(substring(line, k + 1)))
  }
  line
}

# Season codes from an in-tag, e.g. "21=Jahresanfang/Winter"
parse_season_codes <- function(instr) {
  none <- data.frame(code = numeric(0), label = character(0), stringsAsFactors = FALSE)
  if (is.na(instr)) return(none)
  hits <- regmatches(instr, gregexpr("\\b(2[0-9]|3[0-2])\\s*=\\s*[^\n]+", instr, perl = TRUE))[[1]]
  if (length(hits) == 0) return(none)
  data.frame(code = as.numeric(sub("^([0-9]+).*$", "\\1", hits)),
             label = squish(sub("^[0-9]+\\s*=\\s*", "", hits)), stringsAsFactors = FALSE)
}
