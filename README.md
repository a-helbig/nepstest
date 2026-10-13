# nepstest

This package contains functions for testing NEPS Feldddatenlieferungen. The functions specifically tests if ac-tags, af-tags and ra-tags were programmed as intended by comparing values from different variables in the datasets. The most important functions are thus: ac_test, af_test_simple, af_test_complex and date_test_complex. The others are more or less helper functions that facilitate specific things. Review the B180 written test scripts in order to understand how the functions are used in practice.

Install the package with `remotes::install_github('a-helbig/nepstest')`. Note that you might need to install remotes package before.


- `ac_test()`
Tests if target variable equals target value


- `af_test_complex()`
Tests complex af-tag filters


- `af_test_simple()`
Tests for NA equality between var1 and var2


- `ef_test()`
Tests ef-tags (entry filters)


- `label_test()`
Tests the variable labels in the data against the va- and vb-tags of the PV. How the test works is explained in `vignette("label_test", package = "nepstest")`


- `value_label_test()`
Tests the value labels (codes and their labels) in the data against the we-tags of the PV, see the same vignette


- `read_pv()`
Reads a PV (docx) and splits every item into its tags


- `pv_var_labels()`
Expected variable labels from a PV, one row per variable


- `pv_value_labels()`
Expected codes and value labels from a PV, one row per variable and code


- `date_test()`
Compare the values of two date items


- `date_test_complex()`
Tests event or episode date variables


- `expand()`
Expand episode data to monthly structure


- `no_na_in_vars()`
Tests if there are no NA values in var_names


- `only_na_in_vars()`
Tests if there are only NA values in specified var_names


- `replace_season_codes()`
Replace season codes in date variables with corresponding months


- `replace_values_with_na()`
Set specific values to NA


- `var_exists()`
Test if variables are existent in the data
