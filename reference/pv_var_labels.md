# Expected variable labels from a PV

Derives the variable label that each variable should have in the field
data from the va- and vb-tags of a PV. Items whose va-tag holds several
variables, e.g. dates (`etstdat (etstm / etstj)`), multiple choice items
(`ts2352 (ts23525 / ts23526 / ...)`) or lists separated by commas
(`h_etzeit (h_etzeit1, h_etzeit2, ...)`), are split into one row per
variable.

## Usage

``` r
pv_var_labels(pv)
```

## Arguments

- pv:

  The result of
  [`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md),
  or a path (or several paths) to PV docx files.

## Value

A data frame with one row per variable and the columns `var`, `label`
(expected label), `item`, `va`, `vb`, `pv` and `note`.

## Details

The labels in the data are built by the survey institute from the
vb-tag. Brackets and slashes are dropped and every variable of a grouped
item gets the text of the vb-tag with its own part of the list, e.g.

- `etstm` from vb "Startdatum (-monat/-jahr) Erwerbsepisode" becomes
  "Startdatum Monat Erwerbsepisode"

- `etintro1a1` from vb "Interviewerangabe bei Widerspruch (Berufsangabe
  / Berufsartangabe / Zeitpunkt)" becomes "Interviewerangabe bei
  Widerspruch Berufsangabe"

- `h_etzeit1` from vb "Zeitarbeit andauernder Erwerbsepisoden
  (h_etzeit1, h_etzeit2, ...)" becomes "Zeitarbeit andauernder
  Erwerbsepisoden h_etzeit1"

The list in the vb-tag is the first bracket with slashes or commas whose
number of parts equals the number of variables in the va-tag (or, if
there is none, the first bracket with slashes or commas). The parts are
assigned to the variables in order. If the va-tag holds more variables
than the list has parts, which is common for multiple choice items whose
last variables stand for refusal and don't know, the label of the
remaining variables is taken from their line in the we-tag (e.g. the
line `ts23528: verweigert` gives the text of the vb-tag before the list
followed by "verweigert"). Every such case is mentioned in `note`. If no
label can be derived, `label` is NA and `note` says why.

Markers in front of the variable name (e.g. `[ZS]`, `[HILF]`) and `[MF]`
at the start of the vb-tag are removed.

## See also

[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md),
which uses this function. Call it yourself
(`nepstest:::pv_var_labels()`) to see which label a variable is expected
to have.

## Examples

``` r
if (FALSE) { # \dontrun{
labels <- nepstest:::pv_var_labels("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
labels[labels$var %in% c("etstm", "ts23528"), ]
} # }
```
