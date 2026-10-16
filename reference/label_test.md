# Tests variable labels against the PV

Compares the variable labels in a field-data dataframe with the labels
that follow from the va- and vb-tags of the PV (see
[`pv_var_labels()`](https://a-helbig.github.io/nepstest/reference/pv_var_labels.md)).
One call checks all variables of a module.

## Usage

``` r
label_test(
  data,
  pv,
  ignore = c("duration$", "_e$"),
  show_format = FALSE,
  show_unmatched = TRUE,
  stop_on_fail = TRUE
)
```

## Arguments

- data:

  A neps field-data dataframe, read with
  [`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)
  so that the variables carry their labels.

- pv:

  The result of
  [`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md),
  or a path (or several paths) to PV docx files.

- ignore:

  Character vector of regular expressions. Variables in the data whose
  names match are not checked. Default: duration variables and variables
  ending in `_e`.

- show_format:

  Logical, if TRUE the variables whose labels differ only in formatting
  are printed as well.

- show_unmatched:

  Logical, if TRUE (default) the variables that are only in the data or
  only in the PV are listed.

- stop_on_fail:

  Logical, if TRUE (default) the function stops with an error if a label
  differs. If FALSE it only prints the results, which is handy to get
  the result table back.

## Value

Invisibly a data frame with one row per checked variable and the columns
`var`, `pv_var`, `data_label`, `pv_label`, `status` ("ok", "format",
"truncated", "item_ref", "mismatch", "missing_label", "unclear"), `item`
and `note`. If labels differ and `stop_on_fail = TRUE`, the function
stops with an error after printing them.

## Details

Every variable in `data` is matched to a variable of the PV by name.
Loop variables with a suffix like `_1`, `_2` are matched to the PV
variable without the suffix. The label in the data (attribute `label`,
as set by
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html))
is then compared with the expected label:

- identical (apart from blanks): fine.

- different only in brackets, slashes, hyphens, quotes, colons, commas
  or upper/lower case: fine, counted as a formatting difference. Use
  `show_format = TRUE` to list them.

- the data label is cut off at Stata's limit of 80 characters but
  otherwise identical: fine.

- the data label refers to other items by their item number where the PV
  uses the variable name, e.g. a preload with "(23110)" in the data and
  "(bvnr)" in the PV: fine, if the item number belongs to exactly that
  variable in the PV. Counted as an item reference and listed with
  `show_format = TRUE`.

- anything else, or no label at all in the data: ERROR. The function
  prints the variable with both labels and stops at the end.

Variables for which no label could be derived from the PV are listed as
a NOTE with the reason, they do not make the test fail. Variables that
are only in the data or only in the PV are listed as a NOTE as well.
Variables whose names match one of the regular expressions in `ignore`
are skipped.

## See also

Other test functions:
[`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md),
[`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md),
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md),
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md)

## Examples

``` r
if (FALSE) { # \dontrun{
T_26ET <- haven::read_dta("26ET.dta")
pv_26ET <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
label_test(T_26ET, pv_26ET)

# also list the labels that only differ in brackets, hyphens or case
label_test(T_26ET, pv_26ET, show_format = TRUE)

# get the full comparison table without stopping
res <- label_test(T_26ET, pv_26ET, stop_on_fail = FALSE)
res[res$status == "mismatch", ]
} # }
```
