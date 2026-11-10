# Checking field data with nepstest: what to pay attention to

This text is for everyone who writes or reviews test scripts with nepstest. It does not repeat the function
documentation. Instead it describes the points where mistakes happen easily and where a test can pass or fail for the
wrong reason. The getting-started vignette (`vignette("nepstest")`) explains the task and the functions themselves.

## Before you start

Read the field data with `haven::read_dta()`. This keeps the variable and value labels, which the label tests need, and
it leaves the missing codes such as -97 or -98 untouched. Read the PV of the module with `read_pv()`, so you can look up
the tags of each item directly in R.

Keep one script per module and go through the PV in the order of the items. Copy the tag from the PV as a comment
directly above each test. This makes it possible for someone else to compare the PV and the script line by line, and it
helps you later when the PV changes. When a new delivery comes in, it is usually enough to update the tests of the items
that changed since the last delivery, but make sure that changes to a block filter reach every item in that block.

Always use the variable name from the `--va:` tag. Older PVs sometimes refer to other items by their item number in the
filter conditions. Look up the variable name of that item in the PV and use it instead of the number.

## Translating filter conditions

The most important point when testing entry filters with `ef_test()` is how missing values are handled. The CATI and R
treat them differently, and a condition that looks correct can silently test the wrong thing.

In the CATI, a condition like "x not equal 2" is also true when x has no value. In R, `x != 2` returns NA when x is
missing, and `ef_test()` treats an NA condition as "condition not met". If you translate the PV literally, every
respondent with a missing x is therefore tested against the wrong expectation. The correct translation is
`!x %in% 2`, which is TRUE when x is missing, just as in the CATI. This applies to every condition in the PV that is
written as `!=` or `<>`, and it is one of the most common filter conditions in the PVs, so it is worth checking twice.

For the same reason you should not use `==` either. Write `x %in% 1` instead of `x == 1` and `x %in% c(1, 2, 4)` for a
list of values. For conditions on missing values, use `is.na(x)` for "x = ." and `!is.na(x)` for "x <> .".

If `ef_test()` prints a note that the condition evaluates to NA in some rows, treat this as a sign that the condition
was not translated correctly. A correct translation with `%in%` and `is.na()` never produces this note.

Comparisons with `<`, `>`, `<=` or `>=` cannot be written with `%in%`. Here you have to decide explicitly what happens
when the variable is missing, for example `!is.na(x) & x > 5`. It is not yet confirmed how the CATI treats a missing
value in such comparisons, so check this with the person responsible for the PV before relying on such a test.

When an ef-tag has several `if` lines, they are alternatives: the item is asked if any of them holds. Combine them with
`|` and put each line in brackets, especially when a line itself contains `&`. An ef-tag at the start of a block applies
to every item inside that block, in addition to the item's own condition. It is easy to forget this, so either add the
block condition to every test in the block with `&`, or filter the data to the block once and run the tests on the
filtered data. With nested blocks, the conditions of all enclosing blocks apply.

## Reading the results of a filter test

`ef_test()` checks two things: that the item has a value wherever the condition holds, and that it is empty wherever the
condition does not hold. Missing codes like -97 (refused) or -98 (don't know) count as an answer, because the question
was asked. Only an empty value counts as "not asked".

Failures in the first check are often not programming errors but interviews that were broken off before the item was
reached. Look at the printed cases before reporting anything. If many interviews were broken off, it can make sense to
run only the second check with `check = "not_asked"`.

Some items are filled automatically without being shown, for example an end date that is set to the interview date when
an episode is still ongoing. By default, `ef_test()` tolerates such values if the duration variable of the item shows
that it was not displayed. Keep this in mind when a test passes although you expected a value to be flagged.

## Other tests

For autocodings with `ac_test()`, the data must first be restricted to the rows where the autocoding applies. A test on
the unfiltered data fails for rows the autocoding was never meant for. Pass the variables you filtered on as
`print_filter_vars`, so that the printout shows at once why a row failed.

For date ranges with `date_test_complex()`, keep the order of the arguments in mind: lower limit, upper limit, and only
then the dates to be tested. Season codes and missing codes are handled by the function, so you do not need to clean the
dates beforehand.

The label tests compare text. Small differences in spelling or formatting are reported as notes and do not make the test
fail. Read the notes anyway, since some of them point to real differences.

## When a test fails

Every test stops with an error when it finds a problem, so a script run with `source()` stops at the first failure. Run
the script section by section instead.

When a test fails, first check the test itself before you report a problem to the survey institute. Ask yourself
whether the condition was translated correctly, especially with regard to missing values, whether the item lies inside
a block with its own filter, and whether the PV has changed since the last delivery. Only when the test is right and the
printed cases really contradict the PV should the item, the tag and the affected case IDs be reported.
