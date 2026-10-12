# Artificial episode data modelled on the unemployment module (27AL). IDs and values are invented.
# Rules used here (from the PV):
#   aliz:   --ef: if (h_panel = 1)                       within Block 2: --ef: if (alintrop <> 2)
#   aliz2:  --ef: if (h_panel = 0 & h_dauertan = 2)
#   almb:   --ef: if (almod1 = 1, 2, 4)                  within Block 2
#   algb:   --ef: if (almb = 1)
ef_data <- data.frame(
  caseid     = c(1, 2, 3, 4, 5, 6),
  h_panel    = c(1, 1, 1, 0, 0, 0),
  almod1     = c(2, 3, 3, 1, 1, 1),
  alintrop   = c(NA, 1, 2, NA, NA, NA),
  h_dauertan = c(1, 3, NA, 2, 3, 2),
  aliz       = c(1, 2, NA, NA, NA, NA),
  aliz2      = c(NA, NA, NA, 1, NA, -98),
  almb       = c(1, NA, NA, 2, 1, -97),
  algb       = c(2, NA, NA, NA, 1, NA)
)

test_that("ef_test: passes when the data follow the entry filters", {
  expect_no_error(suppressMessages(ef_test(ef_data, "aliz", !alintrop %in% 2 & h_panel == 1)))
  expect_no_error(suppressMessages(ef_test(ef_data, "aliz2", h_panel == 0 & h_dauertan == 2)))
  expect_no_error(suppressMessages(ef_test(ef_data, "almb", !alintrop %in% 2 & almod1 %in% c(1, 2, 4))))
  expect_no_error(suppressMessages(ef_test(ef_data, "algb", almb %in% 1)))
  expect_true(suppressMessages(ef_test(ef_data, "algb", almb %in% 1)))
})

test_that("ef_test: -97 and -98 count as asked", {
  expect_no_error(suppressMessages(ef_test(ef_data, "aliz2", h_panel == 0 & h_dauertan == 2))) # row 6 has aliz2 = -98
})

test_that("ef_test: Test 1 detects underfiltering (not asked although condition is met)", {
  d <- ef_data
  d$aliz[2] <- NA
  expect_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1)), "ef_test failed")
  expect_message(try(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1), silent = TRUE), "Test 1 \\(asked\\): ERROR.*caseid\\(s\\): 2")
})

test_that("ef_test: Test 2 detects overfiltering (asked although condition is not met)", {
  d <- ef_data
  d$aliz[4] <- 1
  expect_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1)), "ef_test failed")
  expect_message(try(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1), silent = TRUE), "Test 2 \\(not asked\\): ERROR.*caseid\\(s\\): 4")
})

test_that("ef_test: check argument restricts the tests", {
  d <- ef_data
  d$aliz[2] <- NA # only Test 1 fails
  expect_no_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1, check = "not_asked")))
  expect_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1, check = "asked")))
  expect_error(ef_test(d, "aliz", h_panel == 1, check = "sometimes"))
})

test_that("ef_test: condition can be given as a string", {
  expect_no_error(suppressMessages(ef_test(ef_data, "algb", "almb %in% 1")))
  cond <- "almb %in% 1"
  expect_no_error(suppressMessages(ef_test(ef_data, "algb", cond)))
  cond_q <- quote(almb %in% 1)
  expect_no_error(suppressMessages(ef_test(ef_data, "algb", cond_q)))
  cond_bad <- quote(almb %in% 2)
  expect_error(suppressMessages(ef_test(ef_data, "algb", cond_bad)))
})

test_that("ef_test: several test_vars with the same condition", {
  d <- ef_data
  d$alendm <- c(NA, 5, NA, 3, 4, 7)
  d$alendj <- c(NA, 2024, NA, 2023, 2024, 2024)
  expect_no_error(suppressMessages(ef_test(d, c("alendm", "alendj"), !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))))
  d$alendj[1] <- 2024
  expect_error(suppressMessages(ef_test(d, c("alendm", "alendj"), !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))))
})

test_that("ef_test: NA in the condition counts as not met and is reported", {
  # alintrop != 2 is NA where alintrop is missing, so those rows count as 'condition not met'
  expect_message(try(ef_test(ef_data, "aliz", alintrop != 2 & h_panel == 1), silent = TRUE), "evaluates to NA in")
  expect_error(suppressMessages(ef_test(ef_data, "aliz", alintrop != 2 & h_panel == 1))) # row 1 is then wrongly treated as filtered out
})

test_that("ef_test: duration variable of 0 or NA tolerates a value in Test 2", {
  d <- ef_data
  d$aliz[4] <- 1
  d$alizduration <- c(5, 4, NA, 0, NA, NA)
  expect_no_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1)))
  d$alizduration[4] <- 3
  expect_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1)))
})

test_that("ef_test: use_duration = FALSE ignores duration variables", {
  d <- ef_data
  d$aliz[4] <- 1
  d$alizduration <- c(5, 4, NA, 0, NA, NA)
  expect_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1, use_duration = FALSE)))
  d$aliz[4] <- NA # without the value the strict test passes again
  expect_no_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1, use_duration = FALSE)))
  expect_error(ef_test(d, "aliz", h_panel == 1, use_duration = "no"), "use_duration")
  expect_error(ef_test(d, "aliz", h_panel == 1, use_duration = NA), "use_duration")
})

test_that("ef_test: autoif-filled end date passes only with the duration rule", {
  # PV: autoif (aliz = 1) alenddat = intm/intj, but alenddat has --ef: if (h_panel = 0) / if (aliz = 2)
  d <- ef_data
  d$alendm <- c(10, 5, NA, 3, 4, 7)                 # row 1: panel case with aliz = 1, filled by autoif
  d$scr_alenddatduration <- c(0, 12, NA, 9, 8, 7)   # not shown in row 1
  d$scr_alendmduration <- d$scr_alenddatduration
  expect_no_error(suppressMessages(ef_test(d, "alendm", !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))))
  expect_error(suppressMessages(ef_test(d, "alendm", !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2), use_duration = FALSE)))
})

test_that("ef_test: empty strings count as NA", {
  d <- ef_data
  d$alcomment <- c("text", "", "", "", "", "")
  expect_no_error(suppressMessages(ef_test(d, "alcomment", caseid == 1)))
  d$alcomment[1] <- ""
  expect_error(suppressMessages(ef_test(d, "alcomment", caseid == 1))) # asked but empty
})

test_that("ef_test: input checks", {
  expect_error(ef_test(ef_data, "alizz", h_panel == 1), "not found")
  expect_error(ef_test(ef_data, "aliz", h_panell == 1), "condition not found")
  expect_error(ef_test(ef_data, "aliz", aliz == 1), "must not be used in its own condition")
  expect_error(ef_test(ef_data, "aliz", h_panel + 1), "logical vector")
  expect_error(ef_test(ef_data, 1, h_panel == 1), "character vector")
})

test_that("ef_test: works with haven labelled data", {
  skip_if_not_installed("haven")
  d <- ef_data
  d$h_panel <- haven::labelled(d$h_panel, c(Erstbefragung = 0, Panelbefragung = 1))
  d$aliz <- haven::labelled(d$aliz, c(ja = 1, nein = 2))
  expect_no_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel == 1)))
  expect_no_error(suppressMessages(ef_test(d, "aliz", !alintrop %in% 2 & h_panel %in% 1)))
})

test_that("ef_test: month and year variables use the duration variable of the date", {
  d <- ef_data
  d$alendm <- c(10, 5, NA, 3, 4, 7)                 # row 1: filled by autoif, not shown
  d$alendj <- c(2024, 2024, NA, 2023, 2024, 2024)
  d$scr_alenddatduration <- c(0, 12, NA, 9, 8, 7)   # only the date has a duration variable
  cond <- quote(!alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))
  expect_no_error(suppressMessages(ef_test(d, c("alendm", "alendj"), cond)))
  expect_message(ef_test(d, "alendm", cond), "scr_alenddatduration")
  expect_error(suppressMessages(ef_test(d, c("alendm", "alendj"), cond, use_duration = FALSE)))
  d$scr_alenddatduration[1] <- 5                    # shown although filtered out -> must fail
  expect_error(suppressMessages(ef_test(d, "alendm", cond)))
})

test_that("find_duration_var: prefers the own duration variable and falls back to the date", {
  expect_equal(find_duration_var("alge", c("alge", "scr_algeduration")), "scr_algeduration")
  expect_equal(find_duration_var("alge", c("algeduration", "scr_algeduration")), "algeduration")
  expect_equal(find_duration_var("alendm", c("scr_alenddatduration")), "scr_alenddatduration")
  expect_equal(find_duration_var("alendj", c("scr_alendjduration", "scr_alenddatduration")), "scr_alendjduration")
  expect_null(find_duration_var("almb", c("scr_alenddatduration")))
  expect_null(find_duration_var("alme", c("scr_aldatduration")))  # no m/j/y suffix -> no fallback
})

test_that("ef_test: objects from the calling environment can be used in the condition", {
  modes <- c(1, 2, 4)
  expect_no_error(suppressMessages(ef_test(ef_data, "almb", !alintrop %in% 2 & almod1 %in% modes)))
  wrong_modes <- c(3)
  expect_error(suppressMessages(ef_test(ef_data, "almb", !alintrop %in% 2 & almod1 %in% wrong_modes)))
  expect_error(ef_test(ef_data, "almb", almod1 %in% not_defined_anywhere), "not found")
  f <- function(d) { my_modes <- c(1, 2, 4); ef_test(d, "almb", !alintrop %in% 2 & almod1 %in% my_modes) }
  expect_no_error(suppressMessages(f(ef_data))) # also works inside a function
})

test_that("ef_test: TRUE as condition for items that are always asked", {
  expect_no_error(suppressMessages(ef_test(ef_data, "almod1", TRUE)))
  d <- ef_data
  d$almod1[2] <- NA
  expect_error(suppressMessages(ef_test(d, "almod1", TRUE)))
})

test_that("ef_test: works with tibbles, without caseid, with 0 rows and in a pipe", {
  skip_if_not_installed("tibble")
  expect_no_error(suppressMessages(ef_test(tibble::as_tibble(ef_data), "algb", almb %in% 1)))
  expect_no_error(suppressMessages(ef_test(ef_data[, -1], "algb", almb %in% 1)))
  d <- ef_data
  d$algb[1] <- NA
  expect_error(suppressMessages(ef_test(tibble::as_tibble(d), "algb", almb %in% 1)))
  expect_error(suppressMessages(ef_test(d[, -1], "algb", almb %in% 1)))
  expect_message(ef_test(ef_data[0, ], "algb", almb %in% 1), "never met")
  expect_no_error(suppressMessages(ef_data |> ef_test("algb", almb %in% 1)))
})
