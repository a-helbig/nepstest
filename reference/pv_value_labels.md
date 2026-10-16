# Expected value labels from a PV

Derives from the we-tags of a PV which codes and value labels each
variable should have in the field data. Grouped items (dates, multiple
choice items, lists) are split into one set of codes per variable, like
in
[`pv_var_labels()`](https://a-helbig.github.io/nepstest/reference/pv_var_labels.md).

## Usage

``` r
pv_value_labels(pv)
```

## Arguments

- pv:

  The result of
  [`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md),
  or a path (or several paths) to PV docx files.

## Value

A data frame with one row per variable and code and the columns `var`,
`code`, `label`, `list` (name of a list like `[Gemeindeliste]` the codes
come from, otherwise NA), `item`, `va` and `pv`. Variables of the PV
without any code (e.g. open questions without buttons) have one row with
`code` NA.

## Details

The lines of a we-tag are read as follows:

- `1: Arbeiter/in` gives code 1 with the label "Arbeiter/in".

- Conditions in front of a code are removed:
  `if (h_etauto = 0) 5: Mitarbeiter` gives code 5.

- `BUTTONS: verweigert (-97), ...` gives code -97 with the label
  "verweigert", and so on. Conditions in the buttons
  (`[if h_panel = 0] bis heute (-5)`) are removed from the label.

- Hints for the interviewer in `<< >>`, `OFFEN:` lines and date fields
  (`Monat ____ Jahr _____`) give no codes.

- Multiple choice items: the lines in square brackets
  (`[0: nicht genannt]`, `[1: genannt]`) are the codes of every variable
  of the item. The lines for the single variables
  (`ts23525 1: Sonderzahlungen ...`) give no codes.

- Dates: the month variable gets the codes of the buttons and the season
  codes from the in-tag (`21=Jahresanfang/Winter`, ...). The other parts
  of the date only get the codes of the buttons.

- A list in square brackets (e.g. `[Gemeindeliste]`) means that the
  codes come from a list that is not in the PV. The name of the list is
  kept in the column `list`, the buttons of the item are read as usual.

- If the same code appears several times with different texts (e.g.
  under different conditions), every text is accepted.

## See also

[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md),
which uses this function. Call it yourself
(`nepstest:::pv_value_labels()`) to see which codes a variable is
expected to have.

## Examples

``` r
if (FALSE) { # \dontrun{
values <- nepstest:::pv_value_labels("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
values[values$var == "etbs", ]
} # }
```
