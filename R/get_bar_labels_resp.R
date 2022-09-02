#' @title Helper function for \code{mk_barplot_resp} and \code{mk_barploth_resp}.
#' Not for external use.
#'
#' @description \code{get_bar_labels_resp} takes in the original data frame, and
#' returns a data frame of labels and their postions for \code{mk_barplot_resp}
#' and \code{mk_barploth_resp}.
#'
#' @param df Data frame of the original data where bar charts are made from.
#' @param gp String. Name of a categorical variable to group the response var.
#' @param resp String. Name of a continuous variable as the response var.
#' @param fillby String. Name of a second categorical variable for sub-dividing
#'               and coloring the bars.
#' @return A data frame of bar labels and their positions.
#' @seealso \code{\link{mk_barplot_resp}} and \code{\link{mk_barploth_resp}}.
get_bar_labels_resp = function(df, gp, resp, fillby, is_resp_pct, show_pct) {
        gp_symb = as.name(gp)
        resp_symb = as.name(resp)
        fillby_symb = as.name(fillby)

        # if (fillby == gp)
        #         stop("fillby var can't be the same as grouping var!")

        if (fillby == gp | fillby == "1") {
                df_summ = df %>% group_by(!!gp_symb) %>%
                        summarise(!!resp_symb := sum(!!resp_symb, na.rm = T),
                                  .groups = 'drop')
        } else {
                df_summ = df %>% group_by(!!gp_symb, !!fillby_symb) %>%
                        summarise(!!resp_symb := sum(!!resp_symb, na.rm = T),
                                  .groups = 'drop_last')
        }

        df_summ %>% mutate(
                EZPLOT_pct = ifelse(rep(is_resp_pct, dplyr::n()),
                                    !!resp_symb,
                                    !!resp_symb / sum(!!resp_symb)),
                EZPLOT_mid = ifelse(rep(show_pct, dplyr::n()),
                                    EZPLOT_pct/2, !!resp_symb / 2))
}

