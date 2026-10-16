# Read a programming template (PV) from a docx file

Reads the item table(s) of one or more PVs (Programmiervorlagen) in docx
format and splits every item into its tags (`--va:`, `--vb:`, `--we:`,
`--ef:` etc.). The result is the basis for
[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md)
and
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md),
but can also be used directly, e.g. to look up the ef-tag of an item.

## Usage

``` r
read_pv(path)
```

## Arguments

- path:

  Path to a PV in docx format. Can also be a character vector of several
  paths, the PVs are then combined.

## Value

A data frame with one row per item and the columns `pv` (file name),
`item` (text of the first table column, usually the item number), one
column per tag (named after the tag, e.g. `va`, `vb`, `we`; NA if the
item has no such tag) and `text` (the full text of the item).

## Details

Every table row that contains at least one tag becomes one row of the
result. The text of a tag runs from its tag line to the next tag line or
to `--end--`. Tags that occur more than once in an item are joined with
a line break. Lines before the first tag are ignored. Non-breaking
spaces are turned into normal spaces and trailing blanks are removed,
otherwise the text is kept as it is in the PV (including markers like
`[ZS]` or `[MF]`).

## See also

[`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md)
and
[`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md),
which compare the data with the PV.

## Examples

``` r
if (FALSE) { # \dontrun{
pv <- read_pv("NEPS_B180_SC6_EP_26ET_V2_2_20250312.docx")
pv[, c("item", "va", "vb")]

# several PVs at once
pv <- read_pv(c("PV_26ET.docx", "PV_27AL.docx"))
} # }
```
