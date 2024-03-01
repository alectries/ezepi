#' ezrd: Risk difference from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated risk difference.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @export
ezrd <- function(x,
                 exposure_var,
                 outcome_var,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95
){
  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{index_exp}} ~ 'exposed',
                           {{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  ezrd.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # calc risk difference from table
  ezrd.fmsb <- fmsb::riskdifference(
    ezrd.df %>%
      filter(exp == 'exposed') %>%
      select(case) %>%
      pull(),
    ezrd.df %>%
      filter(exp == 'unexposed') %>%
      select(case) %>%
      pull(),
    ezrd.df %>%
      filter(exp == 'exposed') %>%
      select(total) %>%
      pull(),
    ezrd.df %>%
      filter(exp == 'unexposed') %>%
      select(total) %>%
      pull(),
    conf.level = conf_lvl
  )

  # pull numbers from fmsb for tibble
  ezrd.res <- as_tibble(data.frame(
    item = c(
      "Risk Difference", "LCI", "UCI", "p-value"
    ),
    result = c(
      ezrd.fmsb %>% unlist() %>% unname() %>% nth(4),
      ezrd.fmsb %>% unlist() %>% unname() %>% nth(2),
      ezrd.fmsb %>% unlist() %>% unname() %>% nth(3),
      ezrd.fmsb %>% unlist() %>% unname() %>% nth(1)
    )
  ))

  return(ezrd.res)
}
