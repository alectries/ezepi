#' ezprev: Prevalence table from a binary outcome variable
#'
#' Returns a tibble with stratified counts by outcome, along with sums and
#' prevalence.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @export
ezprev <- function(x,
                   outcome_var,
                   index_out = 1,
                   ref_out = 0
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
  ## pull vars
  test.out <- x %>%
    pull({{outcome_var}})
  test.df <- data.frame(test.out)
  ## tests
  if(
    class(test.out) == class({{index_out}}) &
    class(test.out) == class({{ref_out}})
  ){
    message(paste0("ezepi: Index outcome value is ", {{index_out}},
                   " and referent outcome value is ", {{ref_out}}))
  } else {
    stop("ezepi: Error: index/referent outcome does not match variable type")
  }
  if(
    test.df %>% filter(test.out == {{index_out}}) %>% summarise(test.out = n()) >= 1 &
    test.df %>% filter(test.out == {{ref_out}}) %>% summarise(test.out = n()) >= 1
  ){
    message("ezepi: Outcome variable set.")
  } else {
    stop("ezepi: Error: index/referent value does not exist in outcome_var")
  }

  # standardize data
  x.df <- x %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  ezprev.df <- x.df %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(prevalence = case / total)

  # output
  return(ezprev.df)
}
