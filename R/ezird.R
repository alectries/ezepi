#' ezird: Rate difference from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated incidence
#' rate difference.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @export
ezird <- function(x,
                  exposure_var,
                  outcome_var,
                  person_time,
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

  # generate person-time table
  pt.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp) %>%
    select(!out) %>%
    summarise(pt = sum({{person_time}}))

  # generate counts table
  case.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case') %>%
    group_by(exp) %>%
    summarise(case = n())

  # join tables
  ezird.df <- case.df %>%
    inner_join(pt.df, by = join_by(exp))

  # calc rate difference from table
  ezird.fmsb <- fmsb::ratedifference(
    ezird.df %>%
      filter(exp == 'exposed') %>%
      select(case) %>%
      pull(),
    ezird.df %>%
      filter(exp == 'unexposed') %>%
      select(case) %>%
      pull(),
    ezird.df %>%
      filter(exp == 'exposed') %>%
      select(pt) %>%
      pull(),
    ezird.df %>%
      filter(exp == 'unexposed') %>%
      select(pt) %>%
      pull(),
    conf.level = conf_lvl
  )

  # pull numbers from fmsb for tibble
  ezird.res <- as_tibble(data.frame(
    item = c(
      "Rate Difference", "LCI", "UCI", "p-value"
    ),
    result = c(
      ezird.fmsb %>% unlist() %>% unname() %>% nth(4),
      ezird.fmsb %>% unlist() %>% unname() %>% nth(2),
      ezird.fmsb %>% unlist() %>% unname() %>% nth(3),
      ezird.fmsb %>% unlist() %>% unname() %>% nth(1)
    )
  ))

  return(ezird.res)
}
