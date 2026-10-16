# Tests simple af-tags (superseded)

This function is designed for testing af-tags that filter from one item
to the following item (under condition x). It checks that `var2` is not
NA wherever `var1` is not NA, optionally under specified conditions. If
any discrepancies are found, it prints the problematic cases and stops
execution.

## Usage

``` r
af_test_simple(
  data,
  var1,
  var2,
  condition_var = NULL,
  operator = "equal",
  condition_value = NULL,
  empty_as_na = TRUE,
  print_filter_vars = NULL
)
```

## Arguments

- data:

  A neps field-data dataframe.

- var1:

  A string that represents the variable where we want to test the
  af-tag.

- var2:

  A string that represents the variable where the af-tag is pointing to.
  Usually the following item.

- condition_var:

  A string representing a variable name to be used as a conditional
  variable.

- operator:

  A string that specifies the operator to be used for the condition.
  Acceptable operators include: `"equal"`, `"unequal"`, `"greater"`,
  `"smaller"`, `"greaterequal"`, `"smallerequal"`, and `"inlist"`.

- condition_value:

  A string or numeric value to be used as the conditional value.

- empty_as_na:

  Logical, if TRUE empty strings ("") in character variables will be
  treated as NA. Default is TRUE.

- print_filter_vars:

  Optional character vector. Additional variable names to be included
  and prioritized in the printed output when errors are detected. This
  is useful for making filtered or relevant variables visible in
  diagnostic printouts.

## Value

Prints a success message if `var2` is never NA where `var1` is not NA.
Otherwise prints the problematic rows and stops with an error.

## Superseded

This function tests exit filters (af-tags) of older PVs. New PVs use
entry filters (ef-tags), use
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md)
for those. The function is kept for PVs that still contain af-tags.

## See also

Other superseded af-tag tests:
[`af_test_complex()`](https://a-helbig.github.io/nepstest/reference/af_test_complex.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage checking var1 and var2 equality of NA presence
af_test_simple(mydata, "var1", "var2")

# With conditional subsetting and custom operator
af_test_simple(mydata, "var1", "var2", condition_var = "group", operator = "equal", condition_value = 1)

# Including additional variables to print on error for visibility
af_test_simple(mydata, "var1", "var2", print_filter_vars = c("filter_var1", "filter_var2"))
} # }
```
