#' Internal function for startup checks
#'
#' @param args A character vector of argument names to check
#' @param env An environment with the contents of arguments passed from the main function
#' @param ... Additional arguments passed to the function
#'
#' @keywords internal
.startup <- function(args, env){

  # Use environment to evaluate
  with(env, {
  # Check for dataset
  if("x" %in% args){
    if(
      rlang::is_empty(x)
    ){
      rlang::abort(
        message = c(
          cli::style_bold("Must specify a dataset!"),
          "i" = "Did you forget to pipe?"
        )
      )
    } else {
      test.df <- data.frame()
    }
  }

  # Check for exposure_var
  if("exposure_var" %in% args){
    if(
      rlang::is_empty(dplyr::select(x, exposure_var))
    ){
      rlang::abort(
        message = c(
          cli::style_bold("Must specify exposure_var!"),
          "x" = "exposure_var not specified or not present in data."
        )
      )
    } else {
      test.exp <- x %>%
        dplyr::pull(exposure_var)
      test.df$test.exp <- test.exp
    }
  }

  # Check for outcome_var
  if("outcome_var" %in% args){
    if(
      rlang::is_empty(dplyr::select(x, outcome_var))
    ){
      rlang::abort(
        message = c(
          cli::style_bold("Must specify outcome_var!"),
          "x" = "outcome_var not specified or not present in data."
        )
      )
    } else {
      test.out <- x %>%
        dplyr::pull(outcome_var)
      test.df$test.out <- test.out
    }
  }

  # Check for person_time
  if("person_time" %in% args){
    if(
      rlang::is_empty(dplyr::select(x, person_time))
    ){
      rlang::abort(
        message = c(
          cli::style_bold("Must specify person_time!"),
          "x" = "person_time not specified or not present in data."
        )
      )
    } else {
      test.pt <- x %>%
        dplyr::pull(person_time)
      test.df$test.pt <- test.pt
    }
  }

  # Check index_exp
  if("index_exp" %in% args){
    if(
      class(test.exp) == class(index_exp) &
      class(test.exp) == class(ref_exp)
    ){
      message(paste0("ezepi: Index exposure value is ", index_exp))
    } else {
      rlang::abort(
        message = c(
          cli::style_bold("Index exposure does not match variable type"),
          "i" = "Is your variable a character variable?"
        )
      )
    }
    if(
      test.df %>% dplyr::filter(test.exp == index_exp) %>% dplyr::summarise(test.exp = dplyr::n()) >= 1
    ){} else {
      rlang::abort(
        message = c(
          cli::style_bold("Index value does not exist in exposure_var"),
          "i" = "Did you forget to set index_exp?"
        )
      )
    }
  }

  # Check ref_exp
  if("ref_exp" %in% args){
    if(
      class(test.exp) == class(ref_exp)
    ){
      message(paste0("ezepi: Referent exposure value is ", ref_exp))
    } else {
      rlang::abort(
        message = c(
          cli::style_bold("Referent exposure does not match variable type"),
          "i" = "Is your variable a character variable?"
        )
      )
    }
    if(
      test.df %>% dplyr::filter(test.exp == ref_exp) %>% dplyr::summarise(test.exp = dplyr::n()) >= 1
    ){} else {
      rlang::abort(
        message = c(
          cli::style_bold("Referent value does not exist in exposure_var"),
          "i" = "Did you forget to set ref_exp?"
        )
      )
    }
  }

  # Check index_out
  if("index_out" %in% args){
    if(
      class(test.out) == class(index_out)
    ){
      message(paste0("Index outcome value is ", index_out))
    } else {
      rlang::abort(
        message = c(
          cli::style_bold("Index outcome does not match variable type"),
          "i" = "Is your variable a character variable?"
        )
      )
    }
    if(
      test.df %>% dplyr::filter(test.out == index_out) %>% dplyr::summarise(test.out = dplyr::n()) >= 1
    ){} else {
      rlang::abort(
        message = c(
          cli::style_bold("Index value does not exist in outcome_var"),
          "i" = "Did you forget to set index_out?"
        )
      )
    }
  }

  # Check ref_out
  if("ref_out" %in% args){
    if(
      class(test.out) == class(ref_out)
    ){
      message(paste0("Referent outcome value is ", ref_out))
    } else {
      rlang::abort(
        message = c(
          cli::style_bold("Referent outcome does not match variable type"),
          "i" = "Is your variable a character variable?"
        )
      )
    }
    if(
      test.df %>% dplyr::filter(test.out == ref_out) %>% dplyr::summarise(test.out = dplyr::n()) >= 1
    ){} else {
      rlang::abort(
        message = c(
          cli::style_bold("Referent value does not exist in outcome_var"),
          "i" = "Did you forget to set ref_out?"
        )
      )
    }
  }

  # Check row_name and data grouping for ezt
  if("row_name" %in% args){
    if(
      !is.null(row_name)
      ){
        if(!is.character(row_name)){
          rlang::abort(
            message = c(
              cli::style_bold("If defined, row_name must be a string"),
              "i" = "Did you use a symbol for row_name?"
            )
          )
        }
      }
    if(
      dplyr::is_grouped_df(x) == TRUE
    ){
      rlang::abort(
        message = c(
          cli::style_bold("Dataset cannot be grouped!"),
          "i" = "Did you ungroup() after tallying?"
        )
      )
    }
  }

  })

  # Success
  return()

}
