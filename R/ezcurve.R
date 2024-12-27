#' ezcurve: Generate an epi curve
#'
#' Returns a ggplot2 ggplot object that, when displayed, is a bar chart of outcomes by their date of occurence.
#'
#' You can follow the ezcurve call with further ggplot2 calls to customize the graph.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @param date_var A variable in x containing the date each outcome occurred.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param start_date The date on which to start the graph. Defaults to NA, which will automatically set the start date.
#' @param end_date The date on which to end the graph. Defaults to NA, which will automatically set the end date.
#' @param title The plot title. Defaults to NULL.
#' @return A ggplot2 object.
#' @importFrom utils modifyList
#' @importFrom magrittr `%>%`
#' @importFrom rlang inform
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
ezcurve <- function(x,
                    outcome_var,
                    date_var,
                    index_out = 1,
                    start_date = NA,
                    end_date = NA,
                    title = NULL
){
  # startup
  `%>%` <- magrittr::`%>%`
  ezepi:::startup(
    c("xdat", "ovar", "dvar", "iout", "srtd", "endd", "titl"),
    utils::modifyList(formals(ezepi::ezcurve), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "ovar", "dvar", "iout"),
    utils::modifyList(formals(ezepi::ezcurve), as.list(match.call()[-1]))
  )

  # keep only cases
  ezcurve.df <- x.df %>%
    dplyr::filter(out == "case") %>%
    dplyr::group_by(date) %>%
    dplyr::tally()

  # remove data outside limits
  if(!is.na(start_date)){
    ezcurve.df <- ezcurve.df %>%
      dplyr::filter(date >= start_date)
    rlang::inform(
      message = c("i" = paste0(cli::style_bold("ezepi:"),
                               " Limiting dates to after ", start_date, "."))
      )
  }
  if(!is.na(end_date)){
    ezcurve.df <- ezcurve.df %>%
      dplyr::filter(date <= end_date)
    rlang::inform(
      message = c("i" = paste0(cli::style_bold("ezepi:"),
                               " Limiting dates to before ", end_date, "."))
    )
  }


  # plot
  ezcurve.res <- ggplot2::ggplot(
    ezcurve.df,
    ggplot2::aes(x = date, y = n)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = paste0(match.call()[4]),
      y = paste0(match.call()[3]),
      title = ifelse(
        is.null(title),
        "Epi Curve",
        title
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5)
    )

  return(ezcurve.res)
}
