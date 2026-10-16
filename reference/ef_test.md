# Tests ef-tags (entry filters)

This function is designed for testing ef-tags, i.e. entry filters that
state under which condition an item is asked. An item with an ef-tag
must be answered (not NA) in every row where its entry condition holds,
and it must be NA in every row where the condition does not hold. The
function checks both directions:

- Test 1 (asked): `test_var` is not NA where `condition` is TRUE. Fails
  reveal underfiltering, i.e. the item was not asked although it should
  have been (or the interview was broken off before the item).

- Test 2 (not asked): `test_var` is NA where `condition` is FALSE. Fails
  reveal overfiltering, i.e. the item was asked although it should not
  have been.

Values like -97 or -98 count as answered, only NA (and empty strings ""
in character variables) count as not asked.

## Usage

``` r
ef_test(
  data,
  test_var,
  condition,
  check = c("both", "asked", "not_asked"),
  use_duration = TRUE,
  print_filter_vars = NULL
)
```

## Arguments

- data:

  A neps field-data dataframe.

- test_var:

  A string with the name of the item that carries the ef-tag. Can also
  be a character vector of several items that share the same entry
  condition.

- condition:

  The entry condition as an unquoted R expression, e.g.
  `h_panel == 1 & !alintrop %in% 2`. Use `TRUE` for items that are
  always asked. Objects from your script (e.g. a vector of codes) can be
  used too. A single string with the expression is accepted as well,
  e.g. `"h_panel == 1"`, and so is a variable that holds a string or a
  quoted expression, e.g. `cond <- quote(h_panel == 1)`, which is handy
  if several items share a condition.

- check:

  Which direction to test: `"both"` (default), `"asked"` (only Test 1)
  or `"not_asked"` (only Test 2). `"not_asked"` can be useful if many
  interviews were broken off and Test 1 is therefore not informative.

- use_duration:

  Logical, if TRUE (default) Test 2 tolerates a value when the
  corresponding duration variable is 0 or NA. If FALSE, Test 2 fails for
  every value where the condition is not met. Has no effect for items
  without a duration variable.

- print_filter_vars:

  Optional character vector. Additional variable names to be included
  and prioritized in the printed output when errors are detected.
  Variables used in `condition` are always printed next to `test_var`.

## Value

Invisibly returns TRUE if all tests pass, otherwise stops with an error
after printing the problematic cases.

## Details

`condition` is an R expression that is evaluated inside `data`, so it
can combine several variables freely. To translate the PV syntax, use:

- `x = 1, 2` becomes `x %in% c(1, 2)`

- `x <> 2` becomes `!x %in% 2` (this is also TRUE if x is missing, like
  in the CATI logic)

- `x <> .` becomes `!is.na(x)` and `x = .` becomes `is.na(x)`

- `&` and `OR` become `&` and `|`. Several `if` lines of one ef-tag are
  alternatives and are combined with `|`.

Prefer `%in%` over `==` and `!=`: comparisons with `==`, `!=`, `<` or
`>` return NA if a variable is missing. Rows where the condition is NA
are treated as "condition not met", and the function prints a note how
many rows that affected.

The ef-tag of an enclosing block (e.g. "Beginn Block 2") applies to all
items in the block. Either add it to `condition` with `&`, or filter
`data` to the block condition beforehand, as you would with the other
test functions.

With `use_duration = TRUE` (default), a value in Test 2 is tolerated if
a corresponding duration variable (`<var>duration`, `scr_<var>duration`
or `src_<var>duration`) exists and is 0 or NA, because this means the
item was not shown, like in
[`af_test_complex()`](https://a-helbig.github.io/nepstest/reference/af_test_complex.md).
This matters for items that are filled by an autoif without being asked,
e.g. an end date that is set to the interview date if the episode is
ongoing. Month and year variables of a date (e.g. `alendm`, `alendj`)
have no duration variable of their own, so the duration variable of the
date is used instead (e.g. `scr_alenddatduration`). The output names the
duration variable that was used. Set `use_duration = FALSE` for a strict
Test 2 that ignores duration variables.

## See also

Other test functions:
[`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md),
[`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md),
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md),
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# aliz: --ef: if (h_panel = 1)
ef_test(T_27AL, "aliz", h_panel == 1)

# aliz2: --ef: if (h_panel = 0 & h_dauertan = 2)
ef_test(T_27AL, "aliz2", h_panel == 0 & h_dauertan == 2)

# almb: --ef: if (almod1 = 1, 2, 4), within Block 2 with --ef: if (alintrop <> 2)
ef_test(T_27AL, "almb", !alintrop %in% 2 & almod1 %in% c(1, 2, 4))

# alenddat: two ef lines are alternatives: if (h_panel = 0) / if (aliz = 2)
ef_test(T_27AL, c("alendm", "alendj"), !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))

# only check for overfiltering
ef_test(T_27AL, "alge", alme %in% 1, check = "not_asked")

# item that is always asked (--ef: alle)
ef_test(T_27AL, "alnr", TRUE)

# codes defined in the script can be used in the condition
panel_modes <- c(2, 3)
ef_test(T_27AL, "alintro12", almod1 %in% panel_modes)

# strict Test 2 without the duration rule
ef_test(T_27AL, "algb", almb %in% 1, use_duration = FALSE)
} # }
```
