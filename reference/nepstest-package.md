# nepstest: Testing NEPS field data deliveries against the PV

During the field period the survey institute sends field data deliveries
(Stata `.dta` files). nepstest checks whether these data follow the
programming templates (PVs, Programmiervorlagen). Every function tests
one kind of tag of the PV and either prints `SUCCESS` or prints the
problematic rows and caseids and stops with an error.

Which function for which tag:

|  |  |  |
|----|----|----|
| PV tag | What is tested | Function |
| ef-tag | entry filter: item asked if and only if the condition holds | [`ef_test()`](https://a-helbig.github.io/nepstest/reference/ef_test.md) |
| ac-tag | autocoding: variable has the expected value | [`ac_test()`](https://a-helbig.github.io/nepstest/reference/ac_test.md) |
| ra-tag (dates) | date lies between two other dates | [`date_test_complex()`](https://a-helbig.github.io/nepstest/reference/date_test_complex.md) |
| vb-tag | variable labels | [`label_test()`](https://a-helbig.github.io/nepstest/reference/label_test.md) |
| we-tag | value labels (codes and their labels) | [`value_label_test()`](https://a-helbig.github.io/nepstest/reference/value_label_test.md) |
| af-tag (older PVs) | exit filter | [`af_test_simple()`](https://a-helbig.github.io/nepstest/reference/af_test_simple.md), [`af_test_complex()`](https://a-helbig.github.io/nepstest/reference/af_test_complex.md) (superseded) |

[`read_pv()`](https://a-helbig.github.io/nepstest/reference/read_pv.md)
reads a PV docx file. Its result is needed for the label tests and is
handy to look up the tags of an item.

Start with
[`vignette("nepstest")`](https://a-helbig.github.io/nepstest/articles/nepstest.md)
for a complete walkthrough of a module check, and see
[`vignette("label_test")`](https://a-helbig.github.io/nepstest/articles/label_test.md)
for how the label tests work.

## Author

**Maintainer**: Alexander Helbig <alexander.helbig@wzb.eu>
([ORCID](https://orcid.org/YOUR-ORCID-ID))
