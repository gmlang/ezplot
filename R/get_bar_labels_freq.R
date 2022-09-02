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

        # if (fillby == var)
        #         stop("fillby var can't be the same as the var to be counted!")

        if (fillby == var | fillby == "1") {
                df_summ = dplyr::rename(dplyr::count(df, !!as.name(var)),
                                        EZPLOT_cnt = n)
        } else {
                df_summ = dplyr::summarise(
                        dplyr::group_by(df, !!as.name(var), !!as.name(fillby)),
                        EZPLOT_cnt = dplyr::n(), .groups = 'drop_last')
        }

        dplyr::mutate(df_summ,
                      EZPLOT_pct = EZPLOT_cnt / sum(EZPLOT_cnt),
                      EZPLOT_mid = ifelse(rep(show_pct, dplyr::n()),
                                          EZPLOT_pct/2, EZPLOT_cnt/2))
}

