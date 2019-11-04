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
#' @param type A string. The type of graph to be drawn. Can be either 'histogram'
#'             or 'density plot'.
#'
#' @return
#' \code{function(xvar, facet_by = NULL, facet_ncol = 1, add_vline_median = TRUE,
#'                add_vline_mean = TRUE, legend_pos = "right", font_size = 14, ...)}
#' \itemize{
#'      \item xvar     : string, name of a continuous variable for x-axis.
#'      \item facet_by : string, name of a categorical variable for grouping
#'                       x and creating facets. Default = NULL.
#'      \item facet_ncol: number of columns when facetting. Default = 1.
#'                      Only works when facet_by is not NULL.
#'      \item add_vline_median: logical, if TRUE, add a vertical line at the
#'                              median of x. Default = TRUE.
#'      \item add_vline_mean: logical, if TRUE, add a vertical line at the mean
#'                            of x. Default = TRUE.
#'      \item legend_pos: string, legend position. Default = "right".
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item ...: other parameters that can be passed into
#'                 `ggplot2::geom_histogram()` or `ggplot2::geom_density()`.
#'                 For example, `binwidth` and `bins` when drawing a histogram.
#'                 Note `binwidth` can be specified as a number or a function
#'                 that calculates width from x. `bins` (Default = 30) is
#'                 overridden by `binwidth`, and you almost always want to avoid
#'                 using its defult value. Andrew Gelman recommends to choose a
#'                 sufficiently large number to show how raw data are distributed
#'                 without any smoothing as happens when estimating density.
#'                 Read this article for details:
#'                 http://andrewgelman.com/2009/10/23/variations_on_t/.
#'                 For another example, `adjust` is a common parameter you want
#'                 to change when drawing a density plot. Like `binwidth` and `bins`
#'                 when drawing a histogram, it controls the smoothness or jerkinees
#'                 when drawing the density curve.
#' }
#' @export
#' @examples inst/examples/ex-mk_histdens.R
mk_histdens = function(df, type = 'histogram') {
        function(xvar, facet_by = NULL, facet_ncol = 1,
                 add_vline_median = TRUE, add_vline_mean = TRUE,
                 legend_pos = "right", font_size = 14, ...) {

                # --- Prep --- #

                # calc summary stats of x for each group as specified by the
                #       facet_by variable
                df_stats = get_summ_stats(df, xvar, facet_by)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, fill = "1"))

                if (type == 'histogram')
                        p = p + geom_histogram(alpha = 0.8, ...)
                if (type == 'density')
                        p = p + geom_density(alpha = 0.8, ...)

                if (add_vline_median)
                        p = p + geom_vline(
                                aes(xintercept = med, color = "median"),
                                data = df_stats, size = 1,
                                linetype = "dashed", show.legend = T)

                if (add_vline_mean)
                        p = p + geom_vline(
                                aes(xintercept = avg, color = "mean"),
                                data = df_stats, size = 1,
                                linetype = "dashed", show.legend = T)

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + facet_wrap(vars(!!as.name(facet_by)),
                                           ncol = facet_ncol,
                                           scales = "free_x")


                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = "Density") + theme_cowplot(font_size)


                # --- Format Legend --- #

                # add legend to indicate median and mean
                p = p + scale_color_manual(
                        name = "",
                        values = c(median = "#fc7d0b", mean = "#a3acb9"),
                        labels = c(median = paste("median:",
                                                  round(df_stats$med, 2)),
                                   mean = paste("mean:",
                                                round(df_stats$avg, 2)))) +
                        # but remove legend of the blue fill of the bars
                        guides(fill = FALSE) +
                        theme(legend.position = legend_pos)

                p

        }
}

