#' moreior: Multiple odds ratios from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, odds, and odds ratios with associated CIs.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @export
moreior <- function(x,
                   exposure_var,
                   outcome_var,
                   ref_exp = 0,
                   index_out = 1,
                   ref_out = 0,
                   conf_lvl = 0.95
){
  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           !is.na({{exposure_var}}) & {{exposure_var}} != {{ref_exp}} ~
                             paste0('exp.', {{exposure_var}}),
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  moreior.df <- x.df %>%
    filter(!is.na(exp)) %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(odds = case / control)

  # calc odds ratios and CIs to add to table
  moreior.res <- moreior.df %>%
    # IOR estimate
    mutate(IOR = as.numeric(fmsb::oddsratio(
      case,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      control,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(control),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'estimate') %>%
      pull(value))) %>%
    # Lower CI for IOR
    mutate(LCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::oddsratio(
      case,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      control,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(control),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'conf.int1') %>%
      pull(value)),
    exp == 'unexposed' ~ 0)) %>%
    # Upper CI for IOR
    mutate(UCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::oddsratio(
      case,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      control,
      moreior.df %>%
        filter(exp == 'unexposed') %>%
        pull(control),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'conf.int2') %>%
      pull(value)),
    exp == 'unexposed' ~ 0))

  return(moreior.res)
}
