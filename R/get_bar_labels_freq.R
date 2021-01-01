#' @title Helper function for \code{mk_barplot_freq}. Not for external use.
#'
#' @description \code{get_bar_labels_freq} takes in the original data frame, and
#' returns a data frame of labels and their postions for \code{mk_barplot_freq}.
#'
#' @param df Data frame of the original data where bar charts are made of.
#' @param var String, name of the categorical variable to be counted.
#' @param fillby String, name of a second categorical variable used for
#'   sub-dividing and coloring the bars.
#' @param show_pct Logical, if TRUE, calculate bars middle position using pct.
#'   Otherwise, calculate bars middle position using count. Default is FALSE.
#'
#' @return A data frame containing bar labels and their positions
#'
#' @seealso \code{\link{mk_barplot_freq}}.
get_bar_labels_freq = function(df, var, fillby, show_pct) {

        if (fillby == var)
                stop("fillby var can't be the same as the var to be counted!")

        if (fillby == "1") {
                df_summ = dplyr::count(df, !!as.name(var))
        } else {
                df_summ = dplyr::summarise(
                        dplyr::group_by(df, !!as.name(var), !!as.name(fillby)),
                        n = dplyr::n())
        }

        dplyr::mutate(df_summ, pct = n / sum(n),
                      mid_pos = ifelse(rep(show_pct, dplyr::n()), pct/2, n/2))
}

