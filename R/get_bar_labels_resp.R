#' @title Helper function for \code{mk_barplot_resp}. Not for external use.
#'
#' @description \code{get_bar_labels_resp} takes in the original data frame, and
#' returns a data frame of labels and their postions for \code{mk_barplot_resp}.
#'
#' @param df data frame of the original data where bar charts are made of.
#' @param gp string, name of a categorical variable to group the response var.
#' @param resp string, name of a continuous variable as the response var.
#' @param fillby string, name of a second categorical variable used for
#'   sub-dividing and coloring the bars.
#'
#' @return a data frame containing bar labels and their positions
#'
#' @seealso \code{\link{mk_barplot_resp}}.
get_bar_labels_resp = function(df, gp, resp, fillby) {

        if (fillby == gp) {
                stop("fillby variable can't be the same as grouping var!")
        } else if (fillby == "1") {
                return(dplyr::summarise(
                        dplyr::group_by(df, !!as.name(gp)),
                        !!as.name(resp) := sum(!!as.name(resp), na.rm = T),
                        mid_pos = !!as.name(resp) / 2
                        )
                       )
        } else {
                return(dplyr::mutate(
                        dplyr::summarise(
                                dplyr::group_by(df, !!as.name(gp),
                                                !!as.name(fillby)),
                                !!as.name(resp) := sum(!!as.name(resp), na.rm=T),
                                mid_pos = !!as.name(resp) / 2
                                ))
                       )
        }
}

