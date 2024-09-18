#' Internal function to standardize person-time
#'
#' @keywords internal

.standardizept <- function(){
  # generate person-time table
  pt.df <- x.df %>%
    dplyr::filter(!is.na(exp)) %>%
    dplyr::filter(!is.na(out)) %>%
    dplyr::group_by(exp) %>%
    dplyr::select(!out) %>%
    dplyr::summarise(pt = sum({{person_time}}))

  # generate counts table
  case.df <- x.df %>%
    dplyr::filter(!is.na(exp)) %>%
    dplyr::filter(out == 'case') %>%
    dplyr::group_by(exp) %>%
    dplyr::summarise(case = n())

  # join tables and return
  case.df %>%
    inner_join(pt.df, by = join_by(exp)) %>%
    return()
}
