# Getting started: checking a field data delivery

This vignette is for whoever takes over the **Felddatenprüfung**. It
explains what the task is, which function of nepstest belongs to which
tag of the PV, and walks through the check of one module.

## The task

During the field period the survey institute sends field data
deliveries: one Stata file (`.dta`) per module, e.g. `26ET.dta` for the
employment episodes. We check whether the data follow the programming
templates (PVs, Programmiervorlagen). Each item in a PV is described by
tags (simplified example):

    --va: aliz
    --vb: Erwerbslosigkeit andauernd
    --ef: if (h_panel = 1)
    --we: 1: ja
          2: nein
          BUTTONS: verweigert (-97), weiß nicht (-98)

Most errors in a delivery show up in four places:

- **Filters**: an item was asked although it should have been skipped,
  or the other way round.
- **Autocodings**: a variable that the CATI fills automatically has the
  wrong value.
- **Ranges**: a date lies outside the allowed range, e.g. before the
  previous interview.
- **Labels**: variable or value labels don’t match the PV.

## Which function for which tag

| PV tag | What is tested | Function |
|----|----|----|
| `--ef:` | item asked if and only if the condition holds | [`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md) |
| `--ac:` | variable has the expected value | [`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md) |
| `--ra:` (dates) | date lies between two other dates | [`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md) |
| `--vb:` | variable labels | [`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md) |
| `--we:` | value labels | [`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md) |
| `--af:` (older PVs) | exit filter | [`af_test_simple()`](https://a-helbig.github.io/nepstest/reference/af_test_simple.md), [`af_test_complex()`](https://a-helbig.github.io/nepstest/reference/af_test_complex.md) |

[`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md)
reads a PV. You need it for the label tests, and it is handy for looking
up the tags of an item.

The af-tests are superseded: older PVs used exit filters (af-tags,
“after this item go to …”), the current PVs use entry filters (ef-tags,
“this item is asked if …”). Use
[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md)
for every PV with ef-tags.

Every test function works the same way: if everything is fine, it prints
a message starting with `SUCCESS`. If not, it prints the problematic
rows (with `caseid` and the relevant variables first) and **stops with
an error** that lists the problematic caseids.

## Setup

``` r

# once
remotes::install_github("a-helbig/nepstest")

library(nepstest)
```

Read the data with
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html).
This keeps the variable and value labels, which the label tests need,
and the missing codes as they are (-97, -98, …).

``` r

T_27AL  <- haven::read_dta("27AL.dta")
pv_27AL <- read_pv("PV_27AL.docx")
```

## Walkthrough: checking one module

The variable names below are examples. Take the real ones from the PV of
the module you are checking.

Keep one script per module and go through the PV item by item. Put the
tag from the PV as a comment above each test, so that the next person
can see what was tested.

### Look up an item in the PV

[`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md)
returns one row per item and one column per tag:

``` r

pv_27AL[pv_27AL$va %in% "aliz", c("item", "va", "ef", "we")]
```

### Filters: `ef_test()`

[`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md)
checks both directions of an entry filter:

- **Test 1 (asked)**: where the condition holds, the item is not NA. A
  failure means the item was not asked although it should have been (or
  the interview was broken off before the item).
- **Test 2 (not asked)**: where the condition does not hold, the item is
  NA. A failure means the item was asked although it should have been
  skipped.

Missing codes like -97 or -98 count as “asked”. Only NA counts as “not
asked”.

The condition is written as R code. Translate the PV syntax like this:

| PV                 | R                        |
|--------------------|--------------------------|
| `x = 1, 2`         | `x %in% c(1, 2)`         |
| `x <> 2`           | `!x %in% 2`              |
| `x = .` / `x <> .` | `is.na(x)` / `!is.na(x)` |
| `&` / `OR`         | `&` / `|`                |
| several `if` lines | combine with `|`         |

Prefer `%in%` over `==`: `x == 1` is NA when `x` is missing, `x %in% 1`
is FALSE.

``` r

# aliz: --ef: if (h_panel = 1)
ef_test(T_27AL, "aliz", h_panel == 1)

# almb: --ef: if (almod1 = 1, 2, 4), inside Block 2 with --ef: if (alintrop <> 2)
ef_test(T_27AL, "almb", !alintrop %in% 2 & almod1 %in% c(1, 2, 4))

# several items with the same condition at once
ef_test(T_27AL, c("alendm", "alendj"), !alintrop %in% 2 & (h_panel %in% 0 | aliz %in% 2))

# --ef: alle
ef_test(T_27AL, "alnr", TRUE)
```

The ef-tag of a block (“Beginn Block 2”) applies to every item in it.
Either add it to each condition with `&` (as for `almb` above), or
filter the data to the block once and test the items on the filtered
data.

If many interviews were broken off, Test 1 produces many failures that
are not programming errors. Use `check = "not_asked"` to run only Test
2. See
[`?ef_test`](https://a-helbig.github.io/nepstest/reference/ef_test.md)
for the rule about duration variables, which matters for items that are
filled automatically without being shown.

### Autocodings: `ac_test()`

[`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md)
checks that a variable has a fixed value, or the same value as another
variable. Restrict the data to the rows where the autocoding applies,
either with [`filter()`](https://rdrr.io/r/stats/filter.html) beforehand
or with the arguments `condition_var`, `operator` and `condition_value`.

``` r

# h_etumf must be 1 where etazv is between 15 and 90
d <- T_26ET[T_26ET$etazv %in% 15:90, ]
ac_test(d, target_var = "h_etumf", target_value = 1, print_filter_vars = "etazv")

# the same with the condition arguments instead of filtering beforehand
ac_test(T_26ET, target_var = "h_etumf", target_value = 1,
        condition_var = "etazv", operator = "inlist", condition_value = 15:90)

# a variable must have the same value as another variable
ac_test(T_26ET, target_var = "var_a", target_value = "var_b")
```

`print_filter_vars` puts the variables you filtered on next to the
tested variable in the printout, so you see at once why a row failed.

### Date ranges: `date_test_complex()`

Date items usually have a ra-tag saying the date must lie between the
previous interview and today. The arguments are month/year pairs, in
this order: lower limit, upper limit, then the date(s) to test.

``` r

# episode: start and end date between previous and current interview
date_test_complex(T_26ET, "intmPRE", "intjPRE", "intm", "intj",
                  "etstm", "etstj", "etendm", "etendj")

# event: only one date
date_test_complex(T_26ET, "intmPRE", "intjPRE", "intm", "intj", "etstm", "etstj")
```

Season codes (21, 24, …) are turned into months and missing codes are
set to NA. A missing month counts as “some time in that year”. The error
message names the check that failed, e.g. `p2 < var2` means that the end
date lies after the upper limit. See
[`?date_test_complex`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md)
for the full list.

### Labels: `label_test()` and `value_label_test()`

One call checks all variables of a module:

``` r

label_test(T_27AL, pv_27AL)
value_label_test(T_27AL, pv_27AL)
```

These tests compare text, so small differences in formatting are
accepted and reported as notes. How the expected labels are derived from
the PV, and how to read the output, is explained in
[`vignette("label_test")`](https://a-helbig.github.io/nepstest/articles/label_test.md).

## When a test fails

Every test stops with an error, so a script that is run with
[`source()`](https://rdrr.io/r/base/source.html) stops at the first
failure. Run the script chunk by chunk instead, or set
`stop_on_fail = FALSE` in the label tests.

When a test fails:

1.  Check the test first. Is the condition translated correctly from the
    PV? Did the PV change since the last delivery? Is the item inside a
    block with its own ef-tag?
2.  Look at the printed rows. The relevant variables come first.
    Interviews that were broken off often explain failures in Test 1 of
    [`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md).
3.  If the data are wrong, write down the item, the tag and the
    problematic caseids, and report them to the survey institute.

## Older PVs with af-tags

For PVs that still use exit filters,
[`af_test_simple()`](https://a-helbig.github.io/nepstest/reference/af_test_simple.md)
tests a filter to the next item and
[`af_test_complex()`](https://a-helbig.github.io/nepstest/reference/af_test_complex.md)
a filter that skips several items. They take the condition as three
arguments (`condition_var`, `operator`, `condition_value`) instead of an
R expression. See their help pages for details.
