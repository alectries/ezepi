#' ezirr: Rate ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated incidence
#' rate ratio.
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
ezirr <- function(x,
                 exposure_var,
                 outcome_var,
                 person_time,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95
){
  # startup
  ## check that required packages are loaded
  if("dplyr" %in% (.packages())){} else {
    stop("ezepi: ezepi requires the tidyverse. Please execute library(tidyverse) or library(ezepi) before continuing.")
  }
  if("fmsb" %in% (.packages())){} else {
    stop("ezepi: ezepi requires fmsb. Please execute library(fmsb) or library(ezepi) before continuing.")
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
  if(
    is_empty(select({{x}}, {{person_time}}))
  ){
    stop("ezepi: Must specify person_time!")
  }
  ## pull vars
  test.exp <- x %>%
    pull({{exposure_var}})
  test.out <- x %>%
    pull({{outcome_var}})
  test.pt <- x %>%
    pull({{person_time}})
  test.df <- data.frame(test.exp, test.out, test.pt)
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
  if(
    !is.na(nth(test.pt, 1))
  ){
    message(paste0("ezepi: Person-time variable set."))
  } else {
    stop("ezepi: Error: person-time variable does not exist or is not set")
  }

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
  ezirr.df <- case.df %>%
    inner_join(pt.df, by = join_by(exp))

  # calc rate ratio from table
  ezirr.fmsb <- fmsb::rateratio(
    ezirr.df %>%
      filter(exp == 'exposed') %>%
      select(case) %>%
      pull(),
    ezirr.df %>%
      filter(exp == 'unexposed') %>%
      select(case) %>%
      pull(),
    ezirr.df %>%
      filter(exp == 'exposed') %>%
      select(pt) %>%
      pull(),
    ezirr.df %>%
      filter(exp == 'unexposed') %>%
      select(pt) %>%
      pull(),
    conf.level = conf_lvl
  )

  # pull numbers from fmsb for tibble
  ezirr.res <- as_tibble(data.frame(
    item = c(
      "Rate Ratio", "LCI", "UCI", "p-value"
    ),
    result = c(
      ezirr.fmsb %>% unlist() %>% unname() %>% nth(4) %>% as.numeric(),
      ezirr.fmsb %>% unlist() %>% unname() %>% nth(2) %>% as.numeric(),
      ezirr.fmsb %>% unlist() %>% unname() %>% nth(3) %>% as.numeric(),
      ezirr.fmsb %>% unlist() %>% unname() %>% nth(1) %>% as.numeric()
    )
  ))

  return(ezirr.res)
}
