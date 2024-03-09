
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezepi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

ezepi (short for Easy Epidemiology) is a package that simplifies the
calculation of common epidemiological measures of occurrence and
association from pre-existing datasets. ezepi is built using the
tidyverse and fmsb.

## How to install

Run the following commands in your R console to install ezepi:

    install.packages("devtools")
    devtools::install_github("alectries/ezepi")

All versions are currently experimental, so you do not need to specify a
branch or version.

You can update ezepi by running `update_packages()` and selecting the
option for ezepi.

## Basic use

All ezepi functions follow a basic format:

    ezepi_function(
      x,              # the dataset to analyze
      exposure_var,   # the categorical exposure variable
      outcome_var,    # the categorical (binary) outcome variable
      person_time,    # the variable noting observation's contributed person-time
      index_exp,      # the value of exposure_var that represents an exposed observation
      ref_exp,        # the value of exposure_var that represents an unexposed observation
      index_out,      # the value of outcome_var that represents a realized outcome
      ref_out,        # the value of outcome_var that represents an unrealized outcome
      conf_lvl        # the confidence level
    )

Not all of these arguments are used in every function, and many values
have defaults. For instance, `person_time` is only included when the
function calculates a rate, rate difference, or rate ratio.
Additionally, many arguments have default values and do not need to be
specified every time: `ref_exp` defaults to 0, and only needs to be set
if the referent exposure value is not 0.

ezepi is written to support piping. You can perform data management
functions on a dataset before piping it into ezepi without problems.

## Functions

ezepi includes two types of functions: “ez” functions and “more”
functions.

### ez functions

ez functions are used when comparing a binary outcome across a single
exposed (or treated, or index) group and a single unexposed (or
untreated, or referent) group. Thus, ez functions require you to specify
`index_exp`. ez functions include:

1.  **ezprev**: Generates a table with counts and prevalence of a
    specified outcome.
2.  **eztbl**: Generates a table with counts, totals, and risks of the
    specified outcome according to exposure status.
3.  **ezir**: Generates a table with case counts, total person-time, and
    rates of the specified outcome according to exposure status.
4.  **ezrd**: Generates a table with an estimate, CI, and p-value for a
    risk difference.
5.  **ezrr**: Generates a table with an estimate, CI, and p-value for a
    risk ratio.
6.  **ezior**: Generates a table with an estimate, CI, and p-value for
    an incidence odds ratio.
7.  **ezird**: Generates a table with an estimate, CI, and p-value for
    an incidence rate difference.
8.  **ezirr**: Generates a table with an estimate, CI, and p-value for
    an indicence rate ratio.

All ez functions generate a tibble, which can be piped into `View()` or
tidyverse functions to extract needed data.

### more functions

In contrast to ez functions, more functions are used to compare a binary
outcome across multiple exposure categories. The exposure variable is
assumed to be disjoint. more functions do *not* require you to specify
`index_exp`, because all values of `exposure_var` other than `ref_exp`
will be considered treatment groups. more functions include:

1.  **moretbl**: Generates a table with counts, totals, and risks of the
    specified outcome according to exposure category.
2.  **morerd**: Generates a table with counts, totals, risks, risk
    differences, and a CI for each exposed group compared with the
    unexposed group.
3.  **morerr**: Generates a table with counts, totals, risks, risk
    ratios, and a CI for each exposed group compared with the unexposed
    group.
4.  **moreior**: Generates a table with counts, totals, risks, incidence
    odds ratios, and a CI for each exposed group compared with the
    unexposed group.

Currently, there is no more function for incidence rate association
measures. This is due to some perplexing issues with fmsb that are still
being sorted out.

## Experimental status

ezepi is currently an experimental package. This means a few things:

1.  It may not work as intended in all circumstances, because testing
    has been minimal.
2.  It will sometimes spit out warnings you cannot solve by cleaning up
    your own code (on odds ratio commands in particular).
3.  Some backend checks have been added (as of `0.0.2`) to ensure proper
    usage. However, you may still get error messages from underlying
    packages, such as dplyr.
4.  ezepi is light on features because more will be added later.
5.  It is not finished, and thus it is not up to modern standards of
    development.

Please feel free to note issues on Github, but understand that you
should not expect a polished, complete package at this point.

### Features in development

- **MH estimation**: Add an option to calculate Mantel-Haenszel
  statistics instead of crude stats.
- **stratified associations**: Add functions (or options on existing
  functions) allowing for stratifying by a second variable
- **moreirr/moreird**: Like `morerr` and `morerd`, but for incidence
  rates.
- **Unit tests**: To ensure long-term functionality, `testthat` will be
  used to make sure the package still works when changes are made. This
  will take considerable time to add, but ezepi will be experimental
  until it is added.

### Possible future features

- **GLMs**: I would like to add support for easy modeling, but this is a
  longer-term initiative.
- **Graphing**: Specifically boxplots for existing more functions and
  charts for GLMs.

### GitHub issues

Please make use of issues to point out bugs, unexpected or unclear
errors, or suggest new features. ezepi is being actively developed, and
I am interested in useful improvements.
