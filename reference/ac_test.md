# Test if a target variable equals a specified value or another variable's value

This function is designed to test ac-tags by comparing values in a
dataframe. It takes a dataframe as input and tests whether the variable
given by `target_var` equals either a fixed value or the values of
another variable in the dataframe (`target_value`). Optionally, a
conditional subset of the data can be specified to restrict the test.

The function reports success if all tested rows meet the condition. If
any rows do not meet the condition, it prints case IDs and relevant
variables (including those specified in `print_filter_vars`) for
debugging, then stops with an error.

## Usage

``` r
ac_test(
  data,
  target_var,
  target_value,
  condition_var = NULL,
  operator = "equal",
  condition_value = NULL,
  print_filter_vars = NULL
)
```

## Arguments

- data:

  A dataframe (typically a neps field-data dataframe).

- target_var:

  A string specifying the name of the variable to test in the dataframe.

- target_value:

  Either:

  - a numeric vector of expected values, or

  - a string specifying the name of another variable in the dataframe
    whose values are compared against `target_var`.

- condition_var:

  Optional string specifying a variable name to subset/filter the data
  before testing.

- operator:

  Optional string specifying the comparison operator used for
  subsetting. One of: `"equal"`, `"unequal"`, `"greater"`, `"smaller"`,
  `"greaterequal"`, `"smallerequal"`, or `"inlist"`. Defaults to
  `"equal"`.

- condition_value:

  Optional numeric or character value used in conjunction with
  `condition_var` and `operator` to subset the data before testing.

- print_filter_vars:

  Optional character vector specifying additional variable names whose
  columns should be prioritized and displayed early in error prints to
  assist debugging (e.g. variables used in
  [`filter()`](https://rdrr.io/r/stats/filter.html) prior to calling
  `ac_test`).

## Value

Invisibly returns NULL. Function either prints a success message or
stops with a detailed error.

## Details

The function expects `caseid` column in the data by default and always
includes it in output. When errors occur, output tables prioritize
columns in the following order: `caseid`, `target_var`, variables in
`print_filter_vars`, then all other columns.

## See also

Other test functions:
[`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md),
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md),
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md),
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple test that h_etumf equals 1
ac_test(mydata, target_var = "h_etumf", target_value = 1)

# Test that h_etumf equals values of another variable h_expected
ac_test(mydata, target_var = "h_etumf", target_value = "h_expected")

# Test on a subset of data where etazv is between 15 and 90, prioritizing etazv in error prints
filtered_data <- mydata %>% filter(etazv %in% c(15:90, -20))
ac_test(filtered_data, target_var = "h_etumf", target_value = 1,
        print_filter_vars = c("etazv"))
} # }
```
