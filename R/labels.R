#' Expected variable labels from a PV
#'
#' @description
#' Derives the variable label that each variable should have in the field data from the va- and vb-tags of a PV. Items
#' whose va-tag holds several variables, e.g. dates (`etstdat (etstm / etstj)`), multiple choice items
#' (`ts2352 (ts23525 / ts23526 / ...)`) or lists separated by commas (`h_etzeit (h_etzeit1, h_etzeit2, ...)`), are split
#' into one row per variable.
#'
#' @details
#' The labels in the data are built by the survey institute from the vb-tag. Brackets and slashes are dropped and every
#' variable of a grouped item gets the text of the vb-tag with its own part of the list, e.g.
#'
#' * `etstm` from vb "Startdatum (-monat/-jahr) Erwerbsepisode" becomes "Startdatum Monat Erwerbsepisode"
#' * `etintro1a1` from vb "Interviewerangabe bei Widerspruch (Berufsangabe / Berufsartangabe / Zeitpunkt)" becomes
#'   "Interviewerangabe bei Widerspruch Berufsangabe"
#' * `h_etzeit1` from vb "Zeitarbeit andauernder Erwerbsepisoden (h_etzeit1, h_etzeit2, ...)" becomes
#'   "Zeitarbeit andauernder Erwerbsepisoden h_etzeit1"
#'
#' The list in the vb-tag is the first bracket with slashes or commas whose number of parts equals the number of variables
#' in the va-tag (or, if there is none, the first bracket with slashes or commas). The parts are assigned to the variables
#' in order. If the va-tag holds more variables than the list has parts, which is common for multiple choice items whose
#' last variables stand for refusal and don't know, the label of the remaining variables is taken from their line in
#' the we-tag (e.g. the line `ts23528: verweigert` gives the text of the vb-tag before the list followed by "verweigert").
#' Every such case is mentioned in `note`.
#' If no label can be derived, `label` is NA and `note` says why.
#'
#' Markers in front of the variable name (e.g. `[ZS]`, `[HILF]`) and `[MF]` at the start of the vb-tag are removed.
#'
#' @param pv The result of [read_pv()], or a path (or several paths) to PV docx files.
#'
#' @return A data frame with one row per variable and the columns `var`, `label` (expected label), `item`, `va`, `vb`,
#'   `pv` and `note`.
#'
#' @examples
#' \dontrun{
#' labels <- nepstest:::pv_var_labels("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
#' labels[labels$var %in% c("etstm", "ts23528"), ]
#' }
#'
#' @seealso [label_test()], which uses this function. Call it yourself (`nepstest:::pv_var_labels()`) to see
#'   which label a variable is expected to have.
#' @keywords internal
pv_var_labels <- function(pv) {
  pv <- as_pv(pv)
  items <- pv[!is.na(pv$va) & nzchar(trimws(pv$va)), , drop = FALSE]
  if (nrow(items) == 0) {
    stop("ERROR: the PV contains no items with a va-tag.")
  }
  vb <- if ("vb" %in% names(items)) items$vb else rep(NA_character_, nrow(items))
  we <- if ("we" %in% names(items)) items$we else rep(NA_character_, nrow(items))

  rows <- lapply(seq_len(nrow(items)), function(i) {
    out <- expand_item_labels(items$va[i], vb[i], we[i])
    out$item <- items$item[i]
    out$va <- items$va[i]
    out$vb <- vb[i]
    out$pv <- items$pv[i]
    out
  })
  out <- do.call(rbind, rows)
  out <- out[, c("var", "label", "item", "va", "vb", "pv", "note")]
  rownames(out) <- NULL
  out
}

#' Tests variable labels against the PV
#'
#' @description
#' Compares the variable labels in a field-data dataframe with the labels that follow from the va- and vb-tags of the PV
#' (see [pv_var_labels()]). One call checks all variables of a module.
#'
#' @details
#' Every variable in `data` is matched to a variable of the PV by name. Loop variables with a suffix like `_1`, `_2` are
#' matched to the PV variable without the suffix. The label in the data (attribute `label`, as set by
#' [haven::read_dta()]) is then compared with the expected label:
#'
#' * identical (apart from blanks): fine.
#' * different only in brackets, slashes, hyphens, quotes, colons, commas or upper/lower case: fine, counted as a formatting
#'   difference. Use `show_format = TRUE` to list them.
#' * the data label is cut off at Stata's limit of 80 characters but otherwise identical: fine.
#' * the data label refers to other items by their item number where the PV uses the variable name, e.g. a preload with
#'   "(23110)" in the data and "(bvnr)" in the PV: fine, if the item number belongs to exactly that variable in the PV.
#'   Counted as an item reference and listed with `show_format = TRUE`.
#' * anything else, or no label at all in the data: ERROR. The function prints the variable with both labels and stops at
#'   the end.
#'
#' Variables for which no label could be derived from the PV are listed as a NOTE with the reason, they do not make the
#' test fail. Variables that are only in the data or only in the PV are listed as a NOTE as well. Variables whose names
#' match one of the regular expressions in `ignore` are skipped.
#'
#' @param data A neps field-data dataframe, read with [haven::read_dta()] so that the variables carry their labels.
#' @param pv The result of [read_pv()], or a path (or several paths) to PV docx files.
#' @param ignore Character vector of regular expressions. Variables in the data whose names match are not checked.
#'   Default: duration variables and variables ending in `_e`.
#' @param show_format Logical, if TRUE the variables whose labels differ only in formatting are printed as well.
#' @param show_unmatched Logical, if TRUE (default) the variables that are only in the data or only in the PV are listed.
#' @param stop_on_fail Logical, if TRUE (default) the function stops with an error if a label differs. If FALSE it only
#'   prints the results, which is handy to get the result table back.
#'
#' @return Invisibly a data frame with one row per checked variable and the columns `var`, `pv_var`, `data_label`,
#'   `pv_label`, `status` ("ok", "format", "truncated", "item_ref", "mismatch", "missing_label", "unclear"), `item` and `note`.
#'   If labels differ and `stop_on_fail = TRUE`, the function stops with an error after printing them.
#'
#' @examples
#' \dontrun{
#' T_26ET <- haven::read_dta("26ET.dta")
#' pv_26ET <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
#' label_test(T_26ET, pv_26ET)
#'
#' # also list the labels that only differ in brackets, hyphens or case
#' label_test(T_26ET, pv_26ET, show_format = TRUE)
#'
#' # get the full comparison table without stopping
#' res <- label_test(T_26ET, pv_26ET, stop_on_fail = FALSE)
#' res[res$status == "mismatch", ]
#' }
#'
#' @family test functions
#' @export label_test
label_test <- function(data, pv, ignore = c("duration$", "_e$"), show_format = FALSE, show_unmatched = TRUE,
                       stop_on_fail = TRUE) {
  data_name <- deparse(substitute(data))
  if (!is.data.frame(data)) {
    stop("ERROR: data must be a dataframe.")
  }
  for (arg in c("show_format", "show_unmatched", "stop_on_fail")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1 || is.na(value)) stop(paste0("ERROR: ", arg, " must be TRUE or FALSE."))
  }

  expected <- if (is.data.frame(pv) && all(c("var", "label") %in% names(pv))) pv else pv_var_labels(pv)
  if (!"note" %in% names(expected)) expected$note <- NA_character_
  if (!"item" %in% names(expected)) expected$item <- NA_character_
  expected <- expected[!duplicated(expected$var), , drop = FALSE]

  data_vars <- names(data)
  if (length(ignore) > 0) {
    ignored <- Reduce(`|`, lapply(ignore, function(p) grepl(p, data_vars)))
    data_vars <- data_vars[!ignored]
  }
  data_labels <- vapply(data_vars, function(v) {
    lab <- attr(data[[v]], "label", exact = TRUE)
    if (is.null(lab) || length(lab) == 0) NA_character_ else as.character(lab)[1]
  }, character(1), USE.NAMES = FALSE)

  base_vars <- sub("_[0-9]+$", "", data_vars)
  pv_var <- ifelse(data_vars %in% expected$var, data_vars,
                   ifelse(base_vars %in% expected$var, base_vars, NA_character_))

  matched <- !is.na(pv_var)
  idx <- match(pv_var[matched], expected$var)
  result <- data.frame(
    var = data_vars[matched],
    pv_var = pv_var[matched],
    data_label = data_labels[matched],
    pv_label = expected$label[idx],
    status = NA_character_,
    item = expected$item[idx],
    note = expected$note[idx],
    stringsAsFactors = FALSE
  )
  result$status <- compare_labels(result$data_label, result$pv_label)

  # Labels that refer to other items by item number (e.g. preloads): translate the item numbers into the variable
  # names of the PV and compare again
  item_lookup <- item_var_lookup(expected)
  check_ref <- result$status == "mismatch"
  if (any(check_ref) && length(item_lookup) > 0) {
    translated <- replace_item_numbers(result$data_label[check_ref], item_lookup)
    status_ref <- compare_labels(translated, result$pv_label[check_ref])
    result$status[check_ref][status_ref %in% c("ok", "format", "truncated")] <- "item_ref"
  }

  only_in_data <- data_vars[!matched]
  only_in_pv <- setdiff(expected$var, pv_var)

  message("Comparing the variable labels of ", nrow(result), " variable(s) in ", data_name, " with the PV.")

  n_status <- function(s) sum(result$status == s)
  any_fail <- FALSE

  fails <- result[result$status == "mismatch", , drop = FALSE]
  if (nrow(fails) > 0) {
    any_fail <- TRUE
    message("ERROR - ", nrow(fails), " variable label(s) differ from the PV:")
    print_label_rows(fails)
  } else {
    message("SUCCESS - no variable label differs from the PV.")
  }

  missing <- result[result$status == "missing_label", , drop = FALSE]
  if (nrow(missing) > 0) {
    any_fail <- TRUE
    message("ERROR - ", nrow(missing), " variable(s) have no label in the data:")
    print_label_rows(missing)
  }

  if (n_status("format") > 0) {
    message("NOTE: ", n_status("format"), " label(s) differ only in brackets, slashes, hyphens, punctuation or case.",
            if (!show_format) " Use show_format = TRUE to list them." else "")
    if (show_format) print_label_rows(result[result$status == "format", , drop = FALSE])
  }
  if (n_status("item_ref") > 0) {
    message("NOTE: ", n_status("item_ref"), " label(s) refer to items by item number where the PV uses the variable name",
            " (the numbers match the PV).", if (!show_format) " Use show_format = TRUE to list them." else "")
    if (show_format) print_label_rows(result[result$status == "item_ref", , drop = FALSE])
  }
  if (n_status("truncated") > 0) {
    message("NOTE: ", n_status("truncated"), " label(s) are cut off at 80 characters in the data, but match otherwise: ",
            paste(result$var[result$status == "truncated"], collapse = ", "))
  }

  unclear <- result[result$status == "unclear", , drop = FALSE]
  if (nrow(unclear) > 0) {
    message("NOTE: for ", nrow(unclear), " variable(s) no label could be derived from the PV, please check by hand:")
    for (i in seq_len(nrow(unclear))) {
      message("  ", unclear$var[i], ": ", unclear$note[i], " (data label: \"", unclear$data_label[i], "\")")
    }
  }
  noted <- result[!is.na(result$note) & result$status != "unclear", , drop = FALSE]
  if (nrow(noted) > 0) {
    message("NOTE: the PV labels of these variable(s) were derived with an extra rule, see the note:")
    for (n in unique(noted$note)) {
      message("  ", paste(unique(noted$pv_var[noted$note == n]), collapse = ", "), ": ", n)
    }
  }

  if (show_unmatched) {
    if (length(only_in_data) > 0) {
      message("NOTE: ", length(only_in_data), " variable(s) are in the data but not in the PV: ",
              paste(only_in_data, collapse = ", "))
    }
    if (length(only_in_pv) > 0) {
      message("NOTE: ", length(only_in_pv), " variable(s) are in the PV but not in the data: ",
              paste(only_in_pv, collapse = ", "))
    }
  }

  attr(result, "only_in_data") <- only_in_data
  attr(result, "only_in_pv") <- only_in_pv

  if (any_fail && stop_on_fail) {
    stop("label_test failed. Please inspect the above labels.")
  }
  invisible(result)
}

# Accept a PV data frame from read_pv() or path(s) to PV docx files
as_pv <- function(pv) {
  if (is.character(pv)) return(read_pv(pv))
  if (is.data.frame(pv) && "va" %in% names(pv)) return(pv)
  stop("ERROR: pv must be the result of read_pv() or a path to a PV docx file.")
}

# Named vector item number -> va-tag (without markers) of all items in the PV, e.g. c("23110" = "bvnr")
item_var_lookup <- function(expected) {
  if (!all(c("item", "va") %in% names(expected))) return(character(0))
  items <- unique(expected[!is.na(expected$item) & !is.na(expected$va), c("item", "va"), drop = FALSE])
  items <- items[!duplicated(items$item) & !items$item %in% items$item[duplicated(items$item)], , drop = FALSE]
  stats::setNames(vapply(items$va, clean_va, character(1), USE.NAMES = FALSE), items$item)
}

# Replace item numbers like 23110 or 23103P1 in labels by the variable names from the lookup
replace_item_numbers <- function(labels, lookup) {
  vapply(labels, function(l) {
    if (is.na(l)) return(l)
    tokens <- unique(regmatches(l, gregexpr("\\b[0-9]{5}[A-Za-z0-9]*\\b", l, perl = TRUE))[[1]])
    for (t in tokens[tokens %in% names(lookup)]) {
      l <- gsub(paste0("\\b", t, "\\b"), lookup[[t]], l, perl = TRUE)
    }
    l
  }, character(1), USE.NAMES = FALSE)
}

# Collapse blanks
squish <- function(x) gsub("\\s+", " ", trimws(x))

# Labels reduced to words: brackets, slashes, hyphens, quotes, colons, commas, semicolons removed, lower case
normalize_label <- function(x) {
  x <- gsub("[\\[\\](){}/,;:\"'\u201c\u201d\u201e\u2018\u2019\u2013\u2014-]", " ", x, perl = TRUE)
  squish(tolower(x))
}

# Status of each data label compared with the expected label
compare_labels <- function(data_label, pv_label) {
  status <- rep("mismatch", length(data_label))
  d <- squish(data_label)
  p <- squish(pv_label)
  dn <- normalize_label(data_label)
  pn <- normalize_label(pv_label)
  truncated <- !is.na(d) & nchar(d) >= 78 & !is.na(pn) & startsWith(pn, dn)
  status[truncated] <- "truncated"
  status[!is.na(dn) & dn == pn] <- "format"
  status[!is.na(d) & d == p] <- "ok"
  status[is.na(data_label) | !nzchar(trimws(data_label))] <- "missing_label"
  status[is.na(pv_label)] <- "unclear"
  status
}

# Print variables with data label and PV label below each other
print_label_rows <- function(rows) {
  width <- max(nchar(rows$var))
  for (i in seq_len(nrow(rows))) {
    item <- if (!is.na(rows$item[i])) paste0("  (item ", rows$item[i], ")") else ""
    message("  ", formatC(rows$var[i], width = -width), item)
    message("    data: ", if (is.na(rows$data_label[i])) "<no label>" else rows$data_label[i])
    message("    PV:   ", rows$pv_label[i])
  }
}

# Clean a va-tag: first line, markers like [ZS] or [HILF] at the start removed, blanks collapsed
clean_va <- function(va) {
  va <- strsplit(va, "\n", fixed = TRUE)[[1]][1]
  va <- gsub("^\\s*(\\[[^]]*\\]\\s*)+", "", va)
  squish(va)
}

# Clean a vb-tag: first line, [MF] at the start removed, blanks collapsed
clean_vb <- function(vb) {
  if (is.na(vb)) return(NA_character_)
  vb <- strsplit(vb, "\n", fixed = TRUE)[[1]][1]
  if (is.na(vb)) return(NA_character_)
  vb <- sub("^\\s*\\[MF\\]\\s*", "", vb, ignore.case = TRUE)
  vb <- squish(vb)
  if (!nzchar(vb)) NA_character_ else vb
}

# Expected labels for the variable(s) of one item
expand_item_labels <- function(va, vb, we) {
  va_clean <- clean_va(va)
  vb_clean <- clean_vb(vb)
  make <- function(var, label, note = NA_character_) {
    data.frame(var = var, label = label, note = note, stringsAsFactors = FALSE)
  }

  m <- regmatches(va_clean, regexec("^([^ (]+)\\s*\\((.*)\\)$", va_clean))[[1]]
  if (length(m) == 0) {
    note <- if (is.na(vb_clean)) "the item has no vb-tag" else NA_character_
    return(make(va_clean, vb_clean, note))
  }

  vars <- trimws(strsplit(m[3], "[/,]")[[1]])
  vars <- vars[nzchar(vars)]
  if (is.na(vb_clean)) {
    return(make(vars, NA_character_, "the item has no vb-tag"))
  }

  group <- find_vb_list(vb_clean, length(vars))
  if (is.null(group)) {
    return(make(vars, NA_character_,
                paste0("the va-tag has ", length(vars), " variables, but the vb-tag has no list in brackets to split")))
  }

  labels <- rep(NA_character_, length(vars))
  note <- NA_character_
  n_common <- min(length(vars), length(group$parts))
  labels[seq_len(n_common)] <- squish(paste(group$before, group$parts[seq_len(n_common)], group$after))

  if (length(group$parts) != length(vars)) {
    note <- paste0("the va-tag has ", length(vars), " variables, the list in the vb-tag has ",
                   length(group$parts), " parts; the parts were assigned in order")
    if (length(vars) > length(group$parts)) {
      extra <- vars[(n_common + 1):length(vars)]
      we_text <- vapply(extra, function(v) we_line_text(we, v), character(1))
      labels[(n_common + 1):length(vars)] <- ifelse(is.na(we_text), NA_character_,
                                                    squish(paste(group$before, we_text, group$after)))
      found <- extra[!is.na(we_text)]
      if (length(found) > 0) {
        note <- paste0(note, ", the labels of ", paste(found, collapse = ", "), " were taken from the we-tag")
      }
      if (any(is.na(we_text))) {
        note <- paste0(note, ", no label found for ", paste(extra[is.na(we_text)], collapse = ", "))
      }
    }
  }
  make(vars, labels, note)
}

# Find the list in brackets in the vb-tag that belongs to the variables of the va-tag.
# Returns the text before and after the bracket and the cleaned parts, or NULL.
find_vb_list <- function(vb, n_vars) {
  pos <- gregexpr("\\(([^()]*)\\)", vb)[[1]]
  if (pos[1] == -1) return(NULL)
  starts <- as.integer(pos)
  ends <- starts + attr(pos, "match.length") - 1
  inner <- substring(vb, starts + 1, ends - 1)
  is_list <- grepl("[/,]", inner)
  if (!any(is_list)) return(NULL)
  parts <- lapply(inner, split_vb_list)
  n_parts <- vapply(parts, length, integer(1))
  candidates <- which(is_list & n_parts == n_vars)
  k <- if (length(candidates) > 0) candidates[1] else which(is_list)[1]
  list(
    before = trimws(substr(vb, 1, starts[k] - 1)),
    after = trimws(substr(vb, ends[k] + 1, nchar(vb))),
    parts = parts[[k]]
  )
}

# Split the inside of a bracket at slashes (or commas, if there is no slash) and clean the parts:
# a leading hyphen is removed and date parts are capitalized ("-monat" becomes "Monat")
split_vb_list <- function(inner) {
  sep <- if (grepl("/", inner, fixed = TRUE)) "/" else ","
  parts <- trimws(strsplit(inner, sep, fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  parts <- sub("^-+\\s*", "", parts)
  is_date <- tolower(parts) %in% c("tag", "monat", "jahr")
  parts[is_date] <- paste0(toupper(substr(parts[is_date], 1, 1)), tolower(substring(parts[is_date], 2)))
  parts
}

# Text of the line for variable `var` in a we-tag, e.g. "ts23528: verweigert" gives "verweigert" and
# "etintro1a5: 5: die ZP hat ..." gives "die ZP hat ...". Returns NA if there is no such line.
we_line_text <- function(we, var) {
  if (is.na(we)) return(NA_character_)
  lines <- strsplit(we, "\n", fixed = TRUE)[[1]]
  var_esc <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", var)
  pattern <- paste0("(^|[\\s)])", var_esc, "(\\s+-?[0-9]+)?\\s*:\\s*(.*)$")
  hit <- grep(pattern, lines, perl = TRUE)
  if (length(hit) == 0) return(NA_character_)
  text <- sub(paste0("^.*?", pattern), "\\3", lines[hit[1]], perl = TRUE)
  text <- sub("^-?[0-9]+\\s*:\\s*", "", text)
  text <- squish(text)
  if (!nzchar(text)) NA_character_ else text
}
