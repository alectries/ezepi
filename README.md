
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
    devtools::install_github("alectries/ezepi@v1.0.0")

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

ezepi includes two main types of functions: “ez” functions and “more”
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

Most ez functions generate a tibble, which can be piped into `View()` or
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
5.  **moreprev**: Generates a table with counts and prevalence for each
    outcome group.

Currently, there is no more function for incidence rate association
measures. This is due to some perplexing issues with fmsb that are still
being sorted out.

### Miscellaneous functions

These functions are useful for general data management.

1.  **ezt**: Transposes (flips the rows and columns of) a dataframe.
    Similar in function to `t`, except it can retain headers more
    easily.
2.  **mutate_rows**: A wrapper for `dplyr::mutate` that allows you to
    generate row data based on existing rows. It uses `mutate` under the
    hood, so the arguments are the same.

### Features in development

- **Cleanup/reorganization:** The current priority is to reorganize the
  source code to make use of subfunctions that are easier to tweak and
  upgrade. Some savvy users may recognize this as good coding practice,
  but I am young and naïve, so I’m just now figuring that out.
- **Better errors:** Once the source is reorganized, I’ll update error
  messages to use rlang (i.e. `abort()`) instead of base R
  (i.e. `stop()`) to report errors.

### Possible future features

- **Naming:** Set names of exposure and outcome variables to be output
  into a table.
- **GLMs**: I would like to add support for easy modeling, but this is a
  longer-term initiative.
- **Graphing**: Specifically boxplots for existing more functions and
  charts for GLMs.

### GitHub issues

Please make use of issues to point out bugs, unexpected or unclear
errors, or suggest new features. ezepi is being actively developed, and
I am interested in useful improvements.
