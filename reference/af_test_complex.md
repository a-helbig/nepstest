# Tests complex af-tags (superseded)

This function is designed for testing af-tags that filter from one item
to another by skipping items in between (under condition x). It tests
for NA equality between `test_var` and `target_var` and additionally
whether all variables in `overfiltered_vars` are NA. If any errors
occur, it prints the problematic cases with specified extra variables if
requested.

## Usage

``` r
af_test_complex(
  data,
  test_var,
  target_var,
  overfiltered_vars,
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

- test_var:

  A string representing the variable where we want to test the af-tag.

- target_var:

  A string representing the variable where the af-tag is pointing to.

- overfiltered_vars:

  A string or character vector of variables that must be NA as part of
  the skipped items.

- condition_var:

  A string representing a variable name to be used as a conditional
  variable.

- operator:

  A string specifying the operator to be used for the condition.
  Acceptable operators include: `"equal"`, `"unequal"`, `"greater"`,
  `"smaller"`, `"greaterequal"`, `"smallerequal"`, and `"inlist"`.

- condition_value:

  A string or numeric value to be used as the conditional value.

- empty_as_na:

  Logical, if TRUE empty strings ("") in character variables will be
  treated as NA. Default is TRUE.

- print_filter_vars:

  Optional character vector of variable names to be included and
  prioritized in the printed output when errors occur. Useful for
  showing filtered or relevant variables clearly.

## Value

Invisibly returns the tested rows (subsetted to the condition and to
rows where `test_var` is not NA) if both tests pass. Otherwise prints
the problematic rows and stops with an error.

## Superseded

This function tests exit filters (af-tags) of older PVs. New PVs use
entry filters (ef-tags), use
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md)
for those. The function is kept for PVs that still contain af-tags.

## See also

Other superseded af-tag tests:
[`af_test_simple()`](https://a-helbig.github.io/nepstest/reference/af_test_simple.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# item a1 with --af: if (a1 = 2) goto a5: a2 to a4 must be NA, a5 must be asked
af_test_complex(mydata, test_var = "a1", target_var = "a5",
                overfiltered_vars = c("a2", "a3", "a4"),
                condition_var = "a1", operator = "equal", condition_value = 2)
} # }
```
