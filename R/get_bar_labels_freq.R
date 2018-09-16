#' @title Helper function for \code{mk_barplot_freq}. Not for external use.
#'
#' @description \code{get_bar_labels_freq} takes in the original data frame, and
#' returns a data frame of labels and their postions for \code{mk_barplot_freq}.
#'
#' @param df data frame of the original data where bar charts are made of.
#' @param var string, name of the categorical variable to be counted.
#' @param fillby string, name of a second categorical variable used for
#'   sub-dividing and coloring the bars.
#' @param show_pct logical, if TRUE, calculate bars middle position using pct.
#'   Otherwise, calculate bars middle position using count. Default is FALSE.
#'
#' @return a data frame containing bar labels and their positions
#'
#' @seealso \code{\link{mk_barplot_freq}}.
get_bar_labels_freq = function(df, var, fillby, show_pct) {

        if (fillby == var) {
                stop("fillby variable can't be the same as the var to be counted!")
        } else if (fillby == "1") {
                return(dplyr::mutate(
                        dplyr::count(df, !!as.name(var)),
                        pct = n / sum(n),
                        mid_pos = ifelse(rep(show_pct, dplyr::n()), pct/2, n/2))
                       )
        } else {
                return(dplyr::mutate(
                        dplyr::summarise(
                                dplyr::group_by(df, !!as.name(var),
                                                !!as.name(fillby)),
                                n = dplyr::n()),
                        pct = n / sum(n),
                        mid_pos = ifelse(rep(show_pct, dplyr::n()), pct/2, n/2))
                       )
        }
}

