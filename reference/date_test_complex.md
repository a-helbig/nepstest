# Tests date ranges (ra-tags) of event or episode dates

Tests whether dates lie inside the range that the ra-tag of a date item
allows. Typically the range is "between the date of the previous
interview and the date of the current interview". The function can test

- an **event**: one date (month and year), e.g. the date of a birth,
  with 7 arguments, or

- an **episode**: a start and an end date, e.g. an employment episode,
  with 9 arguments.

All variables are given as strings. The arguments are positional and
always come in month/year pairs: first the two dates that limit the
range (`pm1`/`pj1` = lower limit, `pm2`/`pj2` = upper limit), then the
date(s) to test (`varm1`/`varj1` = start or event date, `varm2`/`varj2`
= end date of an episode).

## Usage

``` r
date_test_complex(
  data,
  pm1,
  pj1,
  pm2,
  pj2,
  varm1,
  varj1,
  varm2 = NULL,
  varj2 = NULL
)
```

## Arguments

- data:

  A neps field-data dataframe.

- pm1, pj1:

  Month and year variable of the **lower limit** of the range, e.g.
  `"intmPRE"`, `"intjPRE"` (date of the previous interview).

- pm2, pj2:

  Month and year variable of the **upper limit** of the range, e.g.
  `"intm"`, `"intj"` (date of the current interview).

- varm1, varj1:

  Month and year variable of the date to test. For episodes this is the
  start date, e.g. `"ezstm"`, `"ezstj"`.

- varm2, varj2:

  Month and year variable of the end date of an episode, e.g.
  `"ezendm"`, `"ezendj"`. Leave out for events.

## Value

Prints a success message if all checks pass. Otherwise stops with an
error that lists the failed checks and the caseids involved.

## Details

Before the comparison, season codes in the month variables (21, 24, 27,
30, 32) are turned into months (1, 4, 7, 10, 12) and NEPS missing codes
(-99 to -90, -56 to -51, -29 to -22) are set to NA. Rows in which one of
the date variables is NA (i.e. the item was not asked) are dropped.

If only the month is missing (e.g. -97 "verweigert" but a valid year),
the date is treated as "some time in that year": January for the
earliest and December for the latest possible date. This is the same
rule the survey institute uses for the ra-tag checks in the CATI. If the
year is missing, the date is not checked.

The function runs up to six checks and reports every failed check with
the problematic caseids:

- `p1 > p2`: the lower limit lies after the upper limit (should not
  happen, points to a problem with the preloads).

- `var1 > var2`: the episode ends before it starts (episodes only).

- `p1 > var1` / `p1 > var2`: the start or end date lies before the lower
  limit.

- `p2 < var1` / `p2 < var2`: the start or end date lies after the upper
  limit.

The data needs a `caseid` variable.

## See also

Other test functions:
[`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md),
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md),
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md),
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md)

## Examples

``` r
test_data <- data.frame(
  caseid = c(1, 2),
  intmPRE = c(12, 3), intjPRE = c(2023, 2023), # previous interview
  intm = c(10, 11), intj = c(2024, 2024),      # current interview
  ezstm = c(1, 21), ezstj = c(2024, 2024),     # 21 = Jahresanfang/Winter
  ezendm = c(6, -97), ezendj = c(2024, 2024)   # -97 = verweigert
)

# episode: start and end date must lie between the two interviews
date_test_complex(test_data, "intmPRE", "intjPRE", "intm", "intj",
                  "ezstm", "ezstj", "ezendm", "ezendj")
#> [1] "SUCCESS:: All checks passed successfully."

# event: only one date to test
date_test_complex(test_data, "intmPRE", "intjPRE", "intm", "intj", "ezstm", "ezstj")
#> [1] "SUCCESS:: All checks passed successfully."
```
