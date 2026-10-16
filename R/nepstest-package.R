#' nepstest: Testing NEPS field data deliveries against the PV
#'
#' @description
#' During the field period the survey institute sends field data deliveries (Stata `.dta` files). nepstest checks
#' whether these data follow the programming templates (PVs, Programmiervorlagen). Every function tests one kind of
#' tag of the PV and either prints `SUCCESS` or prints the problematic rows and caseids and stops with an error.
#'
#' Which function for which tag:
#'
#' | PV tag | What is tested | Function |
#' |---|---|---|
#' | ef-tag | entry filter: item asked if and only if the condition holds | [ef_test()] |
#' | ac-tag | autocoding: variable has the expected value | [ac_test()] |
#' | ra-tag (dates) | date lies between two other dates | [date_test_complex()] |
#' | vb-tag | variable labels | [label_test()] |
#' | we-tag | value labels (codes and their labels) | [value_label_test()] |
#' | af-tag (older PVs) | exit filter | [af_test_simple()], [af_test_complex()] (superseded) |
#'
#' [read_pv()] reads a PV docx file. Its result is needed for the label tests and is handy to look up the tags of an
#' item.
#'
#' Start with `vignette("nepstest")` for a complete walkthrough of a module check, and see
#' `vignette("label_test")` for how the label tests work.
#'
#' @keywords internal
#' @importFrom utils head
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
