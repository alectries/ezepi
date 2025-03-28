
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezepi

ezepi (short for Easy Epidemiology) is a package that simplifies the
calculation of common epidemiological measures of occurrence and
association from pre-existing datasets, allowing you to get various epi
measures from line-list data without wrangling it first. Most ezepi
functions are wrappers of fmsb.

# How to Install

Run the following commands in your R console to install ezepi:

    install.packages("devtools")
    devtools::install_github("alectries/ezepi")

You can update ezepi by running `devtools::update_packages()` and
selecting the option for ezepi.

## Basic use

Most ezepi functions follow a basic format:

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

ezepi includes two main types of functions: “ez” functions and “more”
functions.

### ez functions

ez functions are used when comparing a binary outcome across a single
exposed (or treated, or index) group and a single unexposed (or
untreated, or referent) group. Thus, ez functions require you to specify
`index_exp`. ez functions include:

1.  **ezprev**: Generates a table with counts and prevalence of a
    specified outcome.
2.  **eztbl**: Generates a table with counts, totals, and risks (or
    odds) of the specified outcome according to exposure status.
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

Most ez functions generate a tibble, which can be piped into `View()` or
tidyverse functions to extract needed data.

### more functions

In contrast to ez functions, more functions are used to compare a binary
outcome across multiple exposure categories. The exposure variable is
assumed to be disjoint. more functions do *not* require you to specify
`index_exp`, because all values of `exposure_var` other than `ref_exp`
will be considered treatment groups and compared to `ref_exp`. more
functions include:

1.  **moreprev**: Generates a table with prevalences of a specified
    outcome.
2.  **moretbl**: Generates a table with counts, totals, and risks (or
    odds) of the specified outcome according to exposure category.
3.  **moreir**: Generates a table with case counts, total person-time,
    and rates of the specified outcome according to exposure category.
4.  **morerd**: Generates a table with risk differences, CIs, and
    p-values for each exposed group compared with the unexposed group.
5.  **morerr**: Generates a table with risk ratios, CIs, and p-values
    for each exposed group compared with the unexposed group.
6.  **moreior**: Generates a table with incidence odds ratios, CIs, and
    p-values for each exposed group compared with the unexposed group.
7.  **moreird**: Generates a table with incidence rate differences, CIs,
    and p-values for each exposed group compared with the unexposed
    group.
8.  **moreirr**: Generates a table with incidence rate ratios, CIs, and
    p-values for each exposed group compared with the unexposed group.

### mh functions

Like ez functions, mh functions compare a binary outcome across a binary
exposed variable. However, it uses a third stratification variable to
perform Mantel-Haenszel pooling. mh functions include:

1.  **mhrd**: Generates a table with an estimate and CI for a
    Mantel-Haenszel risk difference.
2.  **mhrr**: Generates a table with an estimate and CI for a
    Mantel-Haenszel risk ratio.
3.  **mhior**: Generates a table with an estimate and CI for a
    Mantel-Haenszel incidence odds ratio.

### Utility functions

ezepi also includes utility functions that make certain data wrangling
tasks easier.

1.  **ezt**: An alternative to `base::t()`. Transposes tables but
    without as many errors and hangups.
2.  **mutate_rows**: A wrapper for `dplyr::mutate()` that transposes a
    table, implements typical mutate arguments, and transposes it again,
    which allows you to add rows to data as though they were columns.
3.  **ezcurve**: Generates a simple epi curve with ggplot. You can add
    additional ggplot2 functions after the ezcurve call to customize the
    plot.

## Development status

This package is now in stable condition.

### Coming changes

- Mantel-Haenzel implementation.

## Legal

Copyright © 2024 Alec Higgins

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.
