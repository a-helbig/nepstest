# nepstest

This package contains functions for testing NEPS Feldddatenlieferungen. The functions specifically tests if ac-tags, af-tags and ra-tags were programmed as intended by comparing values from different variables in the datasets.

`ac_test()`
Tests if target variable equals target value

`af_test_complex()`
Tests complex af-tag filters

`af_test_simple()`
Tests for NA equality between var1 and var2

`date_test()`
Compare the values of two date items

`expand()`
Expand episode data to monthly structure

`no_na_in_vars()`
Tests if there are no NA values in var_names

`only_na_in_vars()`
Tests if there are only NA values in specified var_names

`replace_season_codes()`
Replace season codes in date variables with corresponding months

`replace_values_with_na()`
Set specific values to NA

`var_exists()`
Test if variables are existent in the data
