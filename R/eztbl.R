#' eztbl: 2x2 contingency table from binary exposure and outcome variables
#'
#' Returns a tibble with stratified counts by exposure and outcome, along with
#' sums and risks by exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @import dplyr
#' @import fmsb
#' @import magrittr
#' @import rlang
#' @import tibble
#' @import tidyr
#' @import tidyselect
#' @export
eztbl <- function(x,
                  exposure_var,
                  outcome_var,
                  index_exp = 1,
                  ref_exp = 0,
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
    is_empty(select({{x}}, {{exposure_var}}))
  ){
    stop("ezepi: Must specify exposure_var!")
  }
  if(
    is_empty(select({{x}}, {{outcome_var}}))
  ){
    stop("ezepi: Must specify outcome_var!")
  }
  ## pull vars
  test.exp <- x %>%
    pull({{exposure_var}})
  test.out <- x %>%
    pull({{outcome_var}})
  test.df <- data.frame(test.exp, test.out)
  ## tests
  if(
    class(test.exp) == class({{index_exp}}) &
    class(test.exp) == class({{ref_exp}})
  ){
    message(paste0("ezepi: Index exposure value is ", {{index_exp}},
                   " and referent exposure value is ", {{ref_exp}}))
  } else {
    stop("ezepi: Error: index/referent exposure does not match variable type")
  }
  if(
    test.df %>% filter(test.exp == {{index_exp}}) %>% summarise(test.exp = n()) >= 1 &
    test.df %>% filter(test.exp == {{ref_exp}}) %>% summarise(test.exp = n()) >= 1
  ){
    message("ezepi: Exposure variable set.")
  } else {
    stop("ezepi: Error: index/referent value does not exist in exposure_var")
  }
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
    mutate(exp = case_when({{exposure_var}} == {{index_exp}} ~ 'exposed',
                           {{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  eztbl.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # add rows
  eztbl.df <- eztbl.df %>%
    ungroup() %>%
    add_row(
      exp = as.character('sum'),
      case = as.numeric(summarise(., sum(case))),
      control = as.numeric(summarise(., sum(control))),
      total = as.numeric(summarise(., sum(total))),
      risk = as.numeric(case / total)
    )

  # output
  return(eztbl.df)
}
