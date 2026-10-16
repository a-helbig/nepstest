# nepstest

nepstest checks NEPS field data deliveries (Felddatenlieferungen) against the programming templates (PVs). Each
function tests one kind of PV tag. It prints `SUCCESS` if the data are fine. Otherwise it prints the problematic rows
and caseids and stops with an error.

## Installation

```r
# install.packages("remotes")
remotes::install_github("a-helbig/nepstest")
```

## Which function for which tag

| PV tag | What is tested | Function |
|---|---|---|
| `--ef:` | entry filter: item asked if and only if the condition holds | `ef_test()` |
| `--ac:` | autocoding: variable has the expected value | `ac_test()` |
| `--ra:` (dates) | date lies between two other dates | `date_test_complex()` |
| `--vb:` | variable labels | `label_test()` |
| `--we:` | value labels (codes and their labels) | `value_label_test()` |
| `--af:` (older PVs) | exit filter, **superseded** by ef-tags | `af_test_simple()`, `af_test_complex()` |

`read_pv()` reads a PV docx file. The label tests need it, and it is handy for looking up the tags of an item.

## Example

```r
library(nepstest)

T_26ET  <- haven::read_dta("26ET.dta")   # keeps variable and value labels
T_27AL  <- haven::read_dta("27AL.dta")
pv_27AL <- read_pv("PV_27AL.docx")

# aliz: --ef: if (h_panel = 1)
ef_test(T_27AL, "aliz", h_panel == 1)

# h_etumf must be 1 where etazv is between 15 and 90
ac_test(T_26ET[T_26ET$etazv %in% 15:90, ], target_var = "h_etumf", target_value = 1)

# start and end date between previous and current interview
date_test_complex(T_26ET, "intmPRE", "intjPRE", "intm", "intj",
                  "etstm", "etstj", "etendm", "etendj")

# all variable and value labels of the module
label_test(T_27AL, pv_27AL)
value_label_test(T_27AL, pv_27AL)
```

## Documentation

* `vignette("nepstest")`: getting started. Covers the task, how to translate PV conditions into R, a walkthrough of a
  module check, and what to do when a test fails.
* `vignette("label_test")`: how the label tests derive the expected labels from the PV.
* `?ef_test`, `?ac_test`, ...: details of each function.
