#' @title Helper function for \code{barplot_yvar}. Not for external use.
#'
#' @description \code{get_bar_labels_yvar} takes in the original data frame, the
#' x, y and fillby variables. It returns a data frame of labels and their
#' postions for \code{barplot_yvar}.
#'
#' @param df data frame of the original data where bar charts are made of.
#' @param xvar string, name of x categorical variable.
#' @param yvar string, name of y continuous variable.
#' @param fillby string, name of a second categorical variable used for
#'   sub-dividing the (aggregated) y values and coloring the bars.
#'
#' @return a data frame containing bar labels and their positions
#'
#' @seealso \code{\link{mk_barplot_yvar}}.
get_bar_labels_yvar = function(df, xvar, yvar, fillby) {

        if (fillby == xvar) {
                stop("fillby variable can't be the same as xvar!")
        } else if (fillby == "1") {
                return(dplyr::summarise(
                        dplyr::group_by(df, !!as.name(xvar)),
                        !!as.name(yvar) := sum(!!as.name(yvar), na.rm = T))
                       )
        } else {
                return(dplyr::mutate(
                        dplyr::summarise(
                                dplyr::group_by(df, !!as.name(xvar),
                                                !!as.name(fillby)),
                                !!as.name(yvar) := sum(!!as.name(yvar), na.rm=T)
                                ))
                       )
        }
}

