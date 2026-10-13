# Uses the artificial PV tests/testthat/fixtures/pv_test.docx (see test-labels.R). Value labels as the survey institute
# writes them: codes with conditions without the condition, buttons as codes, [0: nicht genannt] / [1: genannt] for
# multiple choice variables, season codes for month variables, no value labels for open answers.
pv_path <- test_path("fixtures", "pv_test.docx")

buttons <- c("weiß nicht" = -98, "verweigert" = -97)
correct_values <- list(
  zet1 = c("ZS durchlaufen" = 1),
  h_panel = c("Erstbefragung" = 0, "Panelbefragung" = 1),
  h_etzeit2 = c("ja" = 1, "nein" = 2),
  ts23525 = c("nicht genannt" = 0, "genannt" = 1),
  ts23528 = c("nicht genannt" = 0, "genannt" = 1),
  etintro1a1 = c("nicht genannt" = 0, "genannt" = 1),
  etbs = c(buttons, "keine Beschäftigten" = 0, "Arbeiter/in" = 1, "Angestellte/r" = 2, "Selbstständige/r" = 5,
           "Freie/r Mitarbeiter/in" = 7),
  etendm = c(buttons, "bis heute" = -5, "Jahresanfang/Winter" = 21, "Frühjahr/Ostern" = 24, "Jahresmitte/Sommer" = 27,
             "Herbst" = 30, "Jahresende" = 32),
  etendj = c(buttons, "bis heute" = -5),
  etstj = buttons,
  tasketnr = c("Episode 1" = 1, "Episode 2" = 2)
)

make_value_data <- function(values) {
  d <- as.data.frame(lapply(values, function(v) c(1, 2)), stringsAsFactors = FALSE)
  for (v in names(values)) attr(d[[v]], "labels") <- values[[v]]
  d
}

test_that("pv_value_labels: reads codes, conditions, buttons, multiple choice and season codes", {
  vl <- pv_value_labels(pv_path)
  codes <- function(v) sort(vl$code[vl$var == v])
  label <- function(v, code) vl$label[vl$var == v & vl$code %in% code]
  expect_equal(codes("etbs"), c(-98, -97, 0, 1, 2, 5, 7))
  expect_equal(label("etbs", 5), "Selbstständige/r")
  expect_equal(label("etbs", 0), "keine Beschäftigten")
  expect_equal(label("etbs", 7), "Freie/r Mitarbeiter/in")
  expect_equal(codes("ts23529"), c(0, 1))
  expect_equal(codes("etintro1a2"), c(0, 1))
  expect_equal(codes("etendm"), c(-98, -97, -5, 21, 24, 27, 30, 32))
  expect_equal(label("etendm", 24), "Frühjahr/Ostern")
  expect_equal(codes("etendj"), c(-98, -97, -5))
  expect_equal(codes("h_etzeit3"), c(1, 2))
  expect_true(all(is.na(vl$code[vl$var %in% c("ettrennm", "etnr")])))
})

test_that("value_label_test: passes when the value labels follow the PV", {
  d <- make_value_data(correct_values)
  expect_no_error(suppressMessages(value_label_test(d, pv_path)))
  res <- suppressMessages(value_label_test(d, pv_path))
  expect_true(all(res$status %in% c("ok", "fill_in")))
  expect_equal(res$status[res$var == "tasketnr"], c("fill_in", "fill_in"))
})

test_that("value_label_test: detects a wrong label, a missing code and an extra code", {
  values <- correct_values
  names(values$etbs)[names(values$etbs) == "Angestellte/r"] <- "Angestellter"
  values$h_panel <- c("Erstbefragung" = 0)
  values$zet1 <- c("ZS durchlaufen" = 1, "ZS nicht durchlaufen" = 2)
  d <- make_value_data(values)
  expect_error(suppressMessages(value_label_test(d, pv_path)), "value_label_test failed")
  res <- suppressMessages(value_label_test(d, pv_path, stop_on_fail = FALSE))
  expect_equal(res$status[res$var == "etbs" & res$code == 2], "mismatch")
  expect_equal(res$status[res$var == "h_panel" & res$code == 1], "missing_in_data")
  expect_equal(res$status[res$var == "zet1" & res$code == 2], "not_in_pv")
  expect_message(try(value_label_test(d, pv_path), silent = TRUE), "value labels of 3 variable\\(s\\) differ")
})

test_that("value_label_test: formatting differences are tolerated, character variables are skipped", {
  values <- correct_values
  names(values$etbs)[names(values$etbs) == "Arbeiter/in"] <- "arbeiter in"
  d <- make_value_data(values)
  d$etstj <- c("2019", "2020") # open answer stored as text
  res <- suppressMessages(value_label_test(d, pv_path))
  expect_equal(res$status[res$var == "etbs" & res$code == 1], "format")
  expect_false("etstj" %in% res$var)
  expect_message(value_label_test(d, pv_path), "1 character variable\\(s\\) were skipped")
})

test_that("value_label_test: variables without codes in the PV must not have value labels", {
  d <- make_value_data(list(etnr = c("Episode" = 1)))
  expect_error(suppressMessages(value_label_test(d, pv_path)), "value_label_test failed")
  d <- make_value_data(list(zet1 = c("ZS durchlaufen" = 1)))
  d$etnr <- c(1, 2)
  expect_no_error(suppressMessages(value_label_test(d, pv_path)))
})

test_that("value_label_test: loop variables and the result of pv_value_labels() are accepted", {
  d <- make_value_data(list(h_panel_1 = c("Erstbefragung" = 0, "Panelbefragung" = 1)))
  res <- suppressMessages(value_label_test(d, pv_value_labels(pv_path)))
  expect_equal(unique(res$pv_var), "h_panel")
  expect_true(all(res$status == "ok"))
})

test_that("value_label_test: codes from a list that is not in the PV are a note, the buttons are still checked", {
  vl <- pv_value_labels(pv_path)
  expect_equal(unique(vl$list[vl$var == "etland"]), "[Länderliste]")
  expect_equal(sort(vl$code[vl$var == "etland"]), c(-98, -97, -96))

  countries <- c("weiß nicht" = -98, "verweigert" = -97, "Land nicht in Liste" = -96,
                 "Afghanistan" = 1, "Ägypten" = 2, "Albanien" = 3)
  d <- make_value_data(list(etland = countries))
  res <- suppressMessages(value_label_test(d, pv_path))
  expect_equal(res$status[res$code > 0], rep("list", 3))
  expect_message(value_label_test(d, pv_path), "etland: 3 code\\(s\\) from \\[Länderliste\\]")

  d <- make_value_data(list(etland = countries[-3]))
  expect_error(suppressMessages(value_label_test(d, pv_path)), "value_label_test failed")
})
