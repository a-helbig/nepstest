# Tests value labels against the PV

Compares the value labels (codes and their labels) in a field-data
dataframe with the codes of the we-tags in the PV (see
[`pv_value_labels()`](https://a-helbig.github.io/nepstest/reference/pv_value_labels.md)).
One call checks all variables of a module.

## Usage

``` r
value_label_test(
  data,
  pv,
  ignore = c("duration$", "_e$"),
  show_format = FALSE,
  stop_on_fail = TRUE
)
```

## Arguments

- data:

  A neps field-data dataframe, read with
  [`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html).

- pv:

  The result of
  [`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md),
  or a path (or several paths) to PV docx files.

- ignore:

  Character vector of regular expressions. Variables in the data whose
  names match are not checked. Default: duration variables and variables
  ending in `_e`.

- show_format:

  Logical, if TRUE the labels that differ only in formatting are printed
  as well.

- stop_on_fail:

  Logical, if TRUE (default) the function stops with an error if a
  difference is found. If FALSE it only prints the results and returns
  the result table.

## Value

Invisibly a data frame with one row per variable and code and the
columns `var`, `pv_var`, `code`, `data_label`, `pv_label`, `status`
("ok", "format", "fill_in", "list", "mismatch", "missing_in_data",
"not_in_pv") and `item`. If differences are found and
`stop_on_fail = TRUE`, the function stops with an error after printing
them.

## Details

Variables are matched to the PV like in
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md):
by name, loop variables with a suffix like `_1` by the name without the
suffix, and variables matching `ignore` are skipped. The value labels in
the data are the attribute `labels` as set by
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html),
a named vector with the codes as values and the labels as names.

For every variable and code the function checks:

- the code is in the PV and in the data, and the labels are identical
  (apart from blanks): fine.

- the labels differ only in brackets, slashes, hyphens, quotes, colons,
  commas or upper/lower case: fine, counted as a formatting difference.
  Use `show_format = TRUE` to list them.

- the labels differ otherwise: ERROR.

- the code is in the PV but has no label in the data: ERROR.

- the code has a label in the data but is not in the PV: ERROR.

- the label in the PV is only a fill-in like `<h_etarta1>`: only the
  code is checked, the text is listed as a NOTE.

- the item takes its codes from a list that is not in the PV (e.g. a
  country list): the codes in the data that are not in the PV are not
  checked and counted in a NOTE. The buttons of the item are checked as
  usual.

Character variables (open answers) cannot carry value labels in Stata
and are skipped. The function prints all differences per variable and
stops at the end, like the other test functions.

## See also

Other test functions:
[`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md),
[`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md),
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md),
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md)

## Examples

``` r
if (FALSE) { # \dontrun{
T_26ET <- haven::read_dta("26ET.dta")
pv_26ET <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
value_label_test(T_26ET, pv_26ET)

res <- value_label_test(T_26ET, pv_26ET, stop_on_fail = FALSE)
res[res$status != "ok", ]
} # }
```
