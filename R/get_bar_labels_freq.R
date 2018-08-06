#' @title Helper function for \code{barplot_freq}. Not for external use.
#'
#' @description \code{get_bar_labels_freq} takes in the original data frame, the
#' x and fillby variable. It returns a data frame of labels and their postions
#' for \code{barplot_freq}.
#'
#' @param df data frame of the original data where bar charts are made of.
#' @param xvar string, name of x categorical variable.
#' @param fillby string, name of a second categorical variable used for
#'   sub-dividing the count or percent and coloring the bars.
#' @param show_pct_on_y logical, if TRUE, show percent on y-axis. Otherwise,
#'   show count. Default is FALSE.
#'
#' @return a data frame containing bar labels and their positions
#'
#' @seealso \code{\link{mk_barplot_freq}}.
get_bar_labels_freq = function(df, xvar, fillby, show_pct_on_y) {

        if (fillby == xvar) {
                stop("fillby variable can't be the same as xvar!")
        } else if (fillby == "1") {
                return(dplyr::mutate(
                        dplyr::count(df, !!as.name(xvar)),
                        pct = n / sum(n),
                        mid_pos_y = ifelse(rep(show_pct_on_y, dplyr::n()),
                                           pct/2, n/2))
                       )
        } else {
                return(dplyr::mutate(
                        dplyr::summarise(
                                dplyr::group_by(df, !!as.name(xvar),
                                                !!as.name(fillby)),
                                n = dplyr::n()),
                        pct = n / sum(n),
                        mid_pos_y = ifelse(rep(show_pct_on_y, dplyr::n()),
                                           pct/2, n/2))
                       )
        }
}

