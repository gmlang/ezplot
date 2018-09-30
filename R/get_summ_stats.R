#' @title Helper function for \code{mk_histogram}. Not for external use.
#'
#' @description \code{get_summ_stats} takes in the original data frame, the
#' x and group variables. It returns a data frame of the summary stats (median,
#' mean and etc) of x in each group. Used to add lines at these summary stats
#' for histograms.
#'
#' @param df original data frame.
#' @param xvar string, name of a continuous variable for x-axis.
#' @param facet_by string, name of a categorical variable for grouping x and
#'   creating a facet for each group.
#'
#' @return a data frame containing summary stats for each group.
#'
#' @seealso \code{\link{mk_histogram}}.
get_summ_stats = function(df, xvar, facet_by) {
        if (is.null(facet_by)) {
                return(dplyr::summarise(
                               df,
                               med = median(!!as.name(xvar), na.rm = T),
                               avg = mean(!!as.name(xvar), na.rm = T)
                               )
                       )
        } else {
                return(
                       dplyr::summarise(
                               dplyr::group_by(df, !!as.name(facet_by)),
                               med = median(!!as.name(xvar), na.rm = T),
                               avg = mean(!!as.name(xvar), na.rm = T)
                               )
                )
        }

}


