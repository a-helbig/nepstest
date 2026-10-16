#' Read a programming template (PV) from a docx file
#'
#' @description
#' Reads the item table(s) of one or more PVs (Programmiervorlagen) in docx format and splits every item into its tags
#' (`--va:`, `--vb:`, `--we:`, `--ef:` etc.). The result is the basis for [label_test()] and [value_label_test()], but can
#' also be used directly, e.g. to look up the ef-tag of an item.
#'
#' @details
#' Every table row that contains at least one tag becomes one row of the result. The text of a tag runs from its tag line
#' to the next tag line or to `--end--`. Tags that occur more than once in an item are joined with a line break. Lines
#' before the first tag are ignored. Non-breaking spaces are turned into normal spaces and trailing blanks are removed,
#' otherwise the text is kept as it is in the PV (including markers like `[ZS]` or `[MF]`).
#'
#' @param path Path to a PV in docx format. Can also be a character vector of several paths, the PVs are then combined.
#'
#' @return A data frame with one row per item and the columns `pv` (file name), `item` (text of the first table column,
#'   usually the item number), one column per tag (named after the tag, e.g. `va`, `vb`, `we`; NA if the item has no
#'   such tag) and `text` (the full text of the item).
#'
#' @examples
#' \dontrun{
#' pv <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
#' pv[, c("item", "va", "vb")]
#'
#' # several PVs at once
#' pv <- read_pv(c("PV_26ET.docx", "PV_27AL.docx"))
#' }
#'
#' @seealso [label_test()] and [value_label_test()], which compare the data with the PV.
#' @export read_pv
read_pv <- function(path) {
  if (!is.character(path) || length(path) == 0) {
    stop("ERROR: path must be a character vector with at least one file path.")
  }
  missing_files <- path[!file.exists(path)]
  if (length(missing_files) > 0) {
    stop(paste("ERROR: file(s) not found:", paste(missing_files, collapse = ", ")))
  }

  pvs <- lapply(path, read_pv_single)
  tag_names <- unique(unlist(lapply(pvs, names)))
  pvs <- lapply(pvs, function(df) {
    for (tag in setdiff(tag_names, names(df))) df[[tag]] <- rep(NA_character_, nrow(df))
    df[, tag_names, drop = FALSE]
  })
  out <- do.call(rbind, pvs)
  rownames(out) <- NULL
  out
}

# Read one PV docx and return one row per table row that contains tags
read_pv_single <- function(path) {
  if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
    stop(paste("ERROR: the PV must be a docx file:", path))
  }
  tmp <- tempfile("pv_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  extracted <- tryCatch(utils::unzip(path, files = "word/document.xml", exdir = tmp), error = function(e) character(0))
  if (length(extracted) == 0) {
    stop(paste("ERROR: could not read", path, "- is it a valid docx file?"))
  }

  doc <- xml2::read_xml(extracted)
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  rows <- xml2::xml_find_all(doc, "//w:tbl/w:tr", ns)

  cell_texts <- lapply(rows, function(row) {
    cells <- xml2::xml_find_all(row, "./w:tc", ns)
    vapply(cells, docx_cell_text, character(1), ns = ns)
  })

  records <- list()
  for (cells in cell_texts) {
    has_tags <- grepl("(^|\n)--[a-z]+:", cells)
    if (!any(has_tags)) next
    text <- paste(cells[has_tags], collapse = "\n")
    item <- if (length(cells) > 1 && !has_tags[1]) trimws(cells[1]) else NA_character_
    if (identical(item, "")) item <- NA_character_
    records[[length(records) + 1]] <- c(list(pv = basename(path), item = item), parse_pv_tags(text), list(text = text))
  }
  if (length(records) == 0) {
    stop(paste("ERROR: no items with tags (e.g. --va:) found in", path))
  }

  tag_names <- unique(unlist(lapply(records, names)))
  tag_names <- c("pv", "item", setdiff(tag_names, c("pv", "item", "text")), "text")
  out <- as.data.frame(
    lapply(stats::setNames(tag_names, tag_names), function(tag) {
      vapply(records, function(r) if (is.null(r[[tag]])) NA_character_ else r[[tag]], character(1))
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out
}

# Text of one table cell: paragraphs are separated by line breaks, tabs and breaks inside paragraphs are kept
docx_cell_text <- function(cell, ns) {
  paragraphs <- xml2::xml_find_all(cell, ".//w:p", ns)
  lines <- vapply(paragraphs, function(p) {
    nodes <- xml2::xml_find_all(p, ".//w:t | .//w:tab | .//w:br | .//w:cr", ns)
    parts <- vapply(nodes, function(n) {
      switch(xml2::xml_name(n), t = xml2::xml_text(n), tab = "\t", "\n")
    }, character(1))
    paste(parts, collapse = "")
  }, character(1))
  text <- paste(lines, collapse = "\n")
  gsub("\u00a0", " ", text, fixed = TRUE)
}

# Split the text of an item into its tags. Returns a named list with one string per tag.
parse_pv_tags <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- sub("[ \t]+$", "", lines)
  tags <- list()
  current <- NULL
  buffer <- character(0)

  flush <- function() {
    if (is.null(current)) return(invisible())
    value <- paste(buffer, collapse = "\n")
    value <- sub("^\\s+", "", value)
    value <- sub("\\s+$", "", value)
    tags[[current]] <<- if (is.null(tags[[current]])) value else paste(tags[[current]], value, sep = "\n")
  }

  for (line in lines) {
    if (grepl("^\\s*--end--", line)) {
      flush()
      current <- NULL
      buffer <- character(0)
    } else if (grepl("^\\s*--[a-z]+:", line)) {
      flush()
      current <- sub("^\\s*--([a-z]+):.*$", "\\1", line)
      buffer <- sub("^\\s*--[a-z]+:\\s*", "", line)
    } else if (!is.null(current)) {
      buffer <- c(buffer, line)
    }
  }
  flush()
  tags
}
