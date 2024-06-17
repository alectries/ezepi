#' moreprev: Prevalence table from a categorical outcome variable
#'
#' Returns a tibble with stratified counts by outcome, along with sums and
#' prevalence.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @return A tibble.
#' @export
moreprev <- function(x,
                     outcome_var
){
  # startup
  ## check that required packages are loaded
  if("dplyr" %in% (.packages())){} else {
    stop("ezepi: ezepi requires the tidyverse. Please execute library(tidyverse) or library(ezepi) before continuing.")
  }
  ## check that required vars exist
  if(
    is_empty(select({{x}}, {{outcome_var}}))
  ){
    stop("ezepi: Must specify outcome_var!")
  }

  # standardize data
  x.df <- x %>%
    mutate(out = case_when(!is.na({{outcome_var}}) ~ {{outcome_var}},
                           .default = NA))
  out.list <- x %>%
    filter(!is.na({{outcome_var}})) %>%
    pull({{outcome_var}}) %>%
    unique()

  # generate a table with a total
  moreprev.df <- x.df %>%
    filter(!is.na(out)) %>%
    group_by(out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()), na.rm = TRUE)) %>%
    mutate(across(everything(), ~ . / total,
                  .names = "{col}_prev"),
           total_prev = NULL) %>%
    select(., c(
      sort(names(.)[!(names(.) %in% "total")]),
      total
    ))

  # output
  return(moreprev.df)
}
