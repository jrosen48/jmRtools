# jmRtools

## Description 

This is an `R` package with personal tools and functions. 

## Installation

Because this package is not available on CRAN, to install it, first install the `devtools` package using `install.packages("devtools")`, followed by the function `devtools::install_github("jrosen48/jmRtools")`. After installing the package, use `library(jmRtools)` to load it in each session.

## Functions

- `convert_log_odds(vec)` where `vec` is a vector of values in log odds units, to be converted into odds
- `convert_odds(vec)` where `vec` is a vector of values in odds units, to be converted into probabilities
- `fix_missing(vec, missing_val)` where `vec` is a `vector`, and `missing_val` is a `character` or number (`i.e., a scalar of `numeric` or `integer` type); this returns a `vector` with `missing_val` values replaced with `NA` (adapted from Wickham's Advanced R)
- `l_unique(vec)` where `vec` is a `vector`; returns the number of unique values
- `composite_matrix_maker(df, ...)` where `df` is a `data.frame`, and `...` are any number of columns evaluated using non-standard evaluation (so use unquoted column names); a `matrix` with the columns specified is returned
- `composite_mean_maker(df, ...)` where `df` is a `data.frame`, and `...` are any number of columns evaluated using non-standard evaluation (so use unquoted column names); a `vector` with the mean of the columns specified is returned
- `composite_stat_maker(df, ...)` where `df` is a `data.frame`, and `...` are any number of columns evaluated using non-standard evaluation (so use unquoted column names); a `character` scalar is returned with M(SD), Cronbach's alpha, and split-half reliability.
- `to_compare(network1, network2, to_combine = F)` where `network1` and `network2` are `data.frame`s with `row.names` and `names` for the two modes (or matrices with `row.names` and `col.names` for the two modes) representing two-mode adjacency matrices; modified networks with structural zeroes added for the `row.names` or `names` not present in the other; `to_combine` (optional) adds together the networks
- `center_vector(v)` center the values in the vector `v` to have mean = 0
- `scale_vector(v)` standardize the values in the vector `v` to have SD = 1
- `center_and_scale_vector(v)` center and standardize the values in the vector `v` to have mean = 0 and SD = 1
- `t_tester()` takes the `dv` (for the dependent variable), `fac` (for the factor), and `df` (for the data frame) using raw (unquoted) variable names. Returns the test statistic, p-value, and effect size.
- `tidy_t_test()` a simple wrapper around `t.test()` with the `broom::tidy()` function around it
