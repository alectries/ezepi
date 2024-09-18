.standardize <- function(args){
  x.df <- x %>%
    dplyr::mutate(exp = dplyr::case_when(c("exposure_var", "index_exp") %in% args &
                                           {{exposure_var}} == {{index_exp}} ~ 'exposed',
                                         c("exposure_var", "ref_exp") %in% args &
                                           {{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                                         c("exposure_var", "ref_exp") %in% args &
                                           "index_exp" !%in% args &
                                           !is.na({{exposure_var}}) &
                                           {{exposure_var}} != {{ref_exp}} ~ paste0('exp.', {{exposure_var}}),
                                         .default = NA),
                  out = dplyr::case_when(c("outcome_var", "index_out") %in% args &
                                           {{outcome_var}} == {{index_out}} ~ 'case',
                                         c("outcome_var", "ref_out") %in% args &
                                           {{outcome_var}} == {{ref_out}} ~ 'control',
                                         .default = NA))
}
