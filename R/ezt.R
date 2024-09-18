#' ezt: Transpose a dataset
#'
#' Returns a tibble with horizontal and vertical axes reversed from the original dataset.
#'
#' In some cases, the data you want to be your headers may be included as row names rather than a variable. If this is the case, pipe \code{as_tibble(x, rownames = "names")} into \code{ezt()}.
#'
#' @param x A dataset.
#' @param row_name A string, which will be the name of the left-most column. Defaults to NULL, which will exclude the existing dataset's headers from the new dataset.
#' @param numeric_data Defaults to FALSE. When TRUE, data except the names column will be coerced to numeric.
#' @return A tibble.
#' @import dplyr
#' @import fmsb
#' @import magrittr
#' @import rlang
#' @import tibble
#' @import tidyr
#' @import tidyselect
#' @export

ezt <- function(
    x,
    row_name = NULL,
    numeric_data = FALSE
){
  # setup
  args_env <- new.env()
  args_env$x <- x
  args_env$row_name <- row_name
  args_env$numeric_data <- numeric_data
  local(
    substitute(ezepi:::.startup(args = c("x", "row_name", "numeric_data"))),
    env = args_env
  )

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
    !is.null(row_name)
    ){
      x.df <- x.df %>%
        setNames(slice(x.df, 1)) %>%
        slice(-1)

      ## if numeric_data is true, coerce
      if(numeric_data == TRUE){
      x.df <- x.df %>%
        mutate(across(everything(), as.numeric))
      }

      x.df <- x.df %>%
        add_column(
          {{row_name}} := names(x)[-1],
          .before = 1
        )
  } else if(
    numeric_data == TRUE
  ){
    x.df <- x.df %>%
      setNames(slice(x.df, 1)) %>%
      slice(-1) %>%
      mutate(across(everything(), as.numeric))
  } else {
    x.df <- x.df %>%
      setNames(slice(x.df, 1)) %>%
      slice(-1)
  }

  return(x.df)
}
