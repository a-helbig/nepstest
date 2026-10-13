# Artificial PV (tests/testthat/fixtures/pv_test.docx) modelled on the employment module (26ET). It contains a timestamp,
# a helper variable, a list separated by commas, two dates, a multiple choice item with "verweigert"/"weiß nicht"
# variables, an [MF] item, a date without a list in the vb-tag, an item with a label longer than 80 characters and
# preloads that refer to other items by variable name.
pv_path <- test_path("fixtures", "pv_test.docx")

long_vb <- paste("Eine Bezeichnung, die so lang ist, dass sie in Stata bei achtzig Zeichen abgeschnitten werden muss,",
                 "und zwar hier")

# Labels as the survey institute writes them
correct_labels <- c(
  zet1 = "Zeitstempel 1 Erwerbstätigkeit",
  h_panel = "Hilfsvariable Panel-/Erstbefragung",
  h_etzeit1 = "Zeitarbeit andauernder Erwerbsepisoden h_etzeit1",
  h_etzeit2 = "Zeitarbeit andauernder Erwerbsepisoden h_etzeit2",
  h_etzeit3 = "Zeitarbeit andauernder Erwerbsepisoden h_etzeit3",
  etstm = "Startdatum Monat Erwerbsepisode",
  etstj = "Startdatum Jahr Erwerbsepisode",
  etent1m = "Datum Monat der Entfristung",
  etent1j = "Datum Jahr der Entfristung",
  ts23525 = "Sondervergütung Sonderzahlungen",
  ts23526 = "Sondervergütung Zuschüsse",
  ts23527 = "Sondervergütung Sonstige",
  ts23528 = "Sondervergütung verweigert",
  ts23529 = "Sondervergütung weiß nicht",
  etintro1a1 = "Interviewerangabe bei Widerspruch Berufsangabe",
  etintro1a2 = "Interviewerangabe bei Widerspruch keine Angabe",
  etsingle = substr(long_vb, 1, 80)
)

# Build a small dataframe whose variables carry the given labels, like haven::read_dta() does
make_labelled_data <- function(labels) {
  d <- as.data.frame(lapply(labels, function(l) c(1, 2)), stringsAsFactors = FALSE)
  for (v in names(labels)) attr(d[[v]], "label") <- labels[[v]]
  d
}

test_that("read_pv: reads all items and splits the tags", {
  pv <- read_pv(pv_path)
  expect_true(all(c("pv", "item", "va", "vb", "we", "ef", "text") %in% names(pv)))
  expect_equal(sum(!is.na(pv$va)), 16)
  expect_equal(pv$va[pv$item %in% "26100Z"], "[ZS] zet1")
  expect_equal(pv$ef[pv$item %in% "26181b"], "if (etent1 = 1)")
  ts_we <- pv$we[pv$item %in% "26214"]
  expect_match(ts_we, "^ts23525 1: Sonderzahlungen")
  expect_match(ts_we, "\\[1: genannt\\]$")
  expect_false(grepl("--ef", ts_we))
})

test_that("read_pv: several PVs are combined, missing files stop", {
  pv <- read_pv(c(pv_path, pv_path))
  expect_equal(sum(!is.na(pv$va)), 32)
  expect_error(read_pv("does_not_exist.docx"), "not found")
})

test_that("pv_var_labels: grouped items are split into one row per variable", {
  lab <- pv_var_labels(pv_path)
  get <- function(v) lab$label[lab$var == v]
  expect_equal(get("zet1"), "Zeitstempel 1 Erwerbstätigkeit")
  expect_equal(get("h_panel"), "Hilfsvariable Panel-/Erstbefragung")
  expect_equal(get("h_etzeit2"), "Zeitarbeit andauernder Erwerbsepisoden h_etzeit2")
  expect_equal(get("etstm"), "Startdatum Monat Erwerbsepisode")
  expect_equal(get("etstj"), "Startdatum Jahr Erwerbsepisode")
  expect_equal(get("etent1j"), "Datum Jahr der Entfristung")
  expect_equal(get("ts23526"), "Sondervergütung Zuschüsse")
  expect_equal(get("ts23529"), "Sondervergütung weiß nicht")
  expect_equal(get("etintro1a1"), "Interviewerangabe bei Widerspruch Berufsangabe")
})

test_that("pv_var_labels: unequal lists are noted, items without a list give NA with a note", {
  lab <- pv_var_labels(pv_path)
  expect_match(lab$note[lab$var == "ts23528"], "5 variables.*3 parts.*ts23528, ts23529 were taken from the we-tag")
  expect_true(is.na(lab$note[lab$var == "etstm"]))
  expect_true(all(is.na(lab$label[lab$var %in% c("ettrennm", "ettrennj")])))
  expect_match(lab$note[lab$var == "ettrennm"], "no list in brackets")
})

test_that("label_test: passes when all labels follow the PV", {
  d <- make_labelled_data(correct_labels)
  expect_no_error(suppressMessages(label_test(d, pv_path, show_unmatched = FALSE)))
  res <- suppressMessages(label_test(d, pv_path))
  expect_true(all(res$status %in% c("ok", "truncated")))
  expect_equal(res$status[res$var == "etsingle"], "truncated")
})

test_that("label_test: detects a wrong label and prints both labels", {
  labels <- correct_labels
  labels["ts23526"] <- "Sondervergütung Sonderzahlungen"
  d <- make_labelled_data(labels)
  expect_error(suppressMessages(label_test(d, pv_path)), "label_test failed")
  expect_message(try(label_test(d, pv_path), silent = TRUE), "ERROR - 1 variable label\\(s\\) differ")
  expect_message(try(label_test(d, pv_path), silent = TRUE), "PV:   Sondervergütung Zuschüsse")
})

test_that("label_test: a missing label is an error", {
  d <- make_labelled_data(correct_labels)
  attr(d$etstm, "label") <- NULL
  expect_error(suppressMessages(label_test(d, pv_path)), "label_test failed")
  res <- suppressMessages(label_test(d, pv_path, stop_on_fail = FALSE))
  expect_equal(res$status[res$var == "etstm"], "missing_label")
})

test_that("label_test: differences in brackets, hyphens or case are tolerated", {
  labels <- correct_labels
  labels["h_panel"] <- "Hilfsvariable Panel Erstbefragung"
  labels["etstm"] <- "Startdatum (monat) Erwerbsepisode"
  d <- make_labelled_data(labels)
  res <- suppressMessages(label_test(d, pv_path))
  expect_equal(res$status[res$var %in% c("h_panel", "etstm")], c("format", "format"))
  expect_message(label_test(d, pv_path), "2 label\\(s\\) differ only in brackets")
})

test_that("label_test: loop suffixes, ignored and unmatched variables", {
  labels <- c(correct_labels[c("zet1", "etstm", "etstj")],
              h_panel_1 = "Hilfsvariable Panel-/Erstbefragung",
              h_panel_2 = "Hilfsvariable Panel-/Erstbefragung",
              scr_etstdatduration = "Dauer",
              caseid = "ID")
  d <- make_labelled_data(labels)
  res <- suppressMessages(label_test(d, pv_path))
  expect_equal(res$pv_var[res$var == "h_panel_2"], "h_panel")
  expect_false("scr_etstdatduration" %in% res$var)
  expect_equal(attr(res, "only_in_data"), "caseid")
  expect_true("ts23525" %in% attr(res, "only_in_pv"))
  expect_message(label_test(d, pv_path), "in the data but not in the PV: caseid")
})

test_that("label_test: variables without a derivable PV label are a note, not an error", {
  labels <- c(correct_labels["zet1"], ettrennm = "Trennungsdatum Monat")
  d <- make_labelled_data(labels)
  expect_no_error(suppressMessages(label_test(d, pv_path)))
  expect_message(label_test(d, pv_path), "ettrennm: the va-tag has 2 variables")
})

test_that("label_test: accepts the result of read_pv() and pv_var_labels()", {
  d <- make_labelled_data(correct_labels)
  pv <- read_pv(pv_path)
  expect_no_error(suppressMessages(label_test(d, pv)))
  expect_no_error(suppressMessages(label_test(d, pv_var_labels(pv))))
  expect_error(label_test(d, 42), "pv must be")
})

test_that("label_test: item numbers in the data that match the variable names in the PV are fine", {
  labels <- c(correct_labels["zet1"],
              etnrPRE = "Preload ET-Episodennummer dieser Episode (26110)",
              etstmPRE = "Preload Startdatum Monat ET-Episode (26122)",
              etstjPRE = "Preload Startdatum Jahr ET-Episode (etstdat (etstm / etstj))")
  d <- make_labelled_data(labels)
  res <- suppressMessages(label_test(d, pv_path))
  expect_equal(res$status[res$var %in% c("etnrPRE", "etstmPRE")], c("item_ref", "item_ref"))
  expect_equal(res$status[res$var == "etstjPRE"], "ok")
  expect_message(label_test(d, pv_path), "2 label\\(s\\) refer to items by item number")
})

test_that("label_test: an item number that belongs to another variable is an error", {
  labels <- c(correct_labels["zet1"], etnrPRE = "Preload ET-Episodennummer dieser Episode (26122)")
  d <- make_labelled_data(labels)
  expect_error(suppressMessages(label_test(d, pv_path)), "label_test failed")
})
