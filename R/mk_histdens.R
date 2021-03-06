#' @title Create a function for making publishable ggplot2 histograms or density plots.
#'
#' @description
#' \code{mk_histdens} takes a data frame as input and returns a function for
#' making histograms or density plots of any continuous variable from the data
#' frame. The output function can also produce faceted plots when supplied
#' a categorical variable for grouping.
#'
#' @seealso \code{\link{get_summ_stats}}.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, type = 'histogram', facet_by = NULL, facet_ncol = 1,
#'                add_vline_median = TRUE, add_vline_mean = TRUE,
#'                legend_pos = "right", font_size = 14, ...)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item type. String, either 'histogram' (default) or 'density'.
#'      \item facet_by. String, name of a categorical variable for grouping x
#'      and creating facets. Default = NULL.
#'      \item facet_ncol. Number of columns when facetting. Default = 1. Only
#'      works when facet_by is not NULL.
#'      \item add_vline_median. Logical, if TRUE, add a vertical line at the
#'      median of x. Default = TRUE.
#'      \item add_vline_mean. Logical, if TRUE, add a vertical line at the mean
#'      of x. Default = TRUE.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#'      \item .... Other parameters that can be passed into `ggplot2::geom_histogram()`
#'      or `ggplot2::geom_density()`. For example, `binwidth` and `bins` when
#'      drawing a histogram. Note `binwidth` can be specified as a number or a
#'      function that calculates width from x. `bins` (Default = 30) is
#'      overridden by `binwidth`, and you almost always want to avoid using its
#'      defult value. Andrew Gelman recommends to choose a sufficiently large
#'      number to show how raw data are distributed without any smoothing as
#'      happens when estimating density. Read this article for details:
#'      http://andrewgelman.com/2009/10/23/variations_on_t/. For another example,
#'      `adjust` is a common parameter you want to change when drawing a density
#'      plot. Like `binwidth` and `bins` when drawing a histogram, it controls
#'      the smoothness or jerkinees when drawing the density curve.
#' }
#' @export
#' @examples inst/examples/ex-mk_histdens.R
mk_histdens = function(df) {
        function(xvar, type = 'histogram', facet_by = NULL, facet_ncol = 1,
                 add_vline_median = TRUE, add_vline_mean = TRUE,
                 legend_pos = "right", font_size = 14, ...) {

                # --- Prep --- #

                # calc summary stats of x for each group as specified by the
                #       facet_by variable
                df_stats = get_summ_stats(df, xvar, facet_by)

                # --- Main Plot --- #

                # show kernel estimated density on y-axis even for histogram
                p = ggplot(df, aes_string(xvar, 'stat(density)', fill = "1"))

                if (type == 'histogram') {
                        p = p + geom_histogram(alpha = 0.8, ...)
                } else { # if (type == 'density')
                        p = p + geom_density(alpha = 0.8, ...)
                }

                legend_med = paste("median:", round(df_stats$med, 2))
                legend_avg = paste("mean:", round(df_stats$avg, 2))
                color_med = "#fc7d0b"
                color_avg = '#56A286'
                if (add_vline_median & add_vline_mean) {

                        # add dashed lines at median and mean
                        p = p + geom_vline(aes(xintercept=med, color="median"),
                                           data=df_stats, size=1,
                                           linetype="dashed", show.legend=T) +
                                geom_vline(aes(xintercept=avg, color="mean"),
                                           data=df_stats, size=1,
                                           linetype="dashed", show.legend=T)

                        # add legend to show the values of the median and mean
                        p = p + scale_color_manual(
                                name = "",
                                values = c(median=color_med, mean=color_avg),
                                labels = c(median=legend_med, mean=legend_avg))

                } else if (add_vline_median){
                        # add dashed lines at the median
                        p = p + geom_vline(aes(xintercept=med, color="median"),
                                           data=df_stats, size=1,
                                           linetype="dashed", show.legend=T) +
                                # add legend to show the median value
                                scale_color_manual(name = "",
                                                   values=c(median=color_med),
                                                   labels=c(median=legend_med))
                } else if (add_vline_mean) {
                        # add dashed lines at the mean
                        p = p + geom_vline(aes(xintercept=avg, color="mean"),
                                           data = df_stats, size = 1,
                                           linetype="dashed", show.legend=T) +
                                # add legend to show the mean value
                                scale_color_manual(name = "",
                                                   values=c(mean = color_avg),
                                                   labels=c(mean = legend_avg))
                } else { }

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + facet_wrap(vars(!!as.name(facet_by)),
                                           ncol = facet_ncol,
                                           scales = "free_x")


                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = 'Density') + theme_cowplot(font_size)


                # --- Format Legend --- #

                # remove legend of the blue fill of the bars
                p = p + guides(fill = 'none') +
                        theme(legend.position = legend_pos)

                p
        }
}

