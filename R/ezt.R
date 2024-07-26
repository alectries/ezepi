#' ezt: Transpose a dataset
#'
#' Returns a tibble with horizontal and vertical axes reversed from the original dataset.
#'
#' In some cases, the data you want to be your headers may be included as row names rather than a variable. If this is the case, pipe \code{as_tibble(x, rownames = "names")} into \code{ezt()}.
#'
#' @param x A dataset.
#' @param row_name A string, which will be the name of the left-most column. Defaults to NULL, which will exclude the existing dataset's headers from the new dataset.
#' @return A tibble.
#' @export

ezt <- function(
    x,
    row_name = NULL
){
  # setup
  ## check that required packages are loaded
  if("dplyr" %in% (.packages())){} else {
    stop("ezepi: ezepi requires the tidyverse. Please execute library(tidyverse) or library(ezepi) before continuing.")
  }
  if("fmsb" %in% (.packages())){} else {
    stop("ezepi: ezepi requires fmsb. Please execute library(fmsb) or library(ezepi) before continuing.")
  }
  ## check that required vars exist
  if(
    is_empty({{x}})
  ){
    stop("ezepi: Must specify a dataset!")
  }
  if(
    !is.null(row_name)
  ){
    if(!is.character({{row_name}})){
      stop("ezepi: If not null, row_name must be a string.")
    }
  }
  ## check if data is grouped
  if(
    is_grouped_df({{x}}) == TRUE
  ){
    stop("ezepi: Dataset cannot be grouped!")
  }

  # transposing
  ## set counter variable
  counter <- nrow(x)

  ## create last column
  name <- as.character(counter)
  x.df <- tibble({{name}} := slice(x, {{counter}}) %>%
                   unlist() %>%
                   as.vector())
  counter <- counter - 1

  ## create other columns
  repeat{
    if(counter == 0){
      break
    } else {
      name <- as.character(counter)
      x.df <- add_column(
        x.df,
        {{name}} := slice(x, {{counter}}) %>%
          unlist() %>%
          as.vector(),
        .before = 1
      )
      counter <- counter - 1
    }
  }

  ## if row_name is specified, create names column
  if(
    is.null(row_name)
    ){
      return(x.df)
    } else {
      x.df <- x.df %>%
        setNames(slice(x.df, 1)) %>%
        slice(-1) %>%
        mutate(across(everything(), as.numeric)) %>%
        add_column(
          {{row_name}} := names(x)[-1],
          .before = 1
        )
      return(x.df)
    }
}
