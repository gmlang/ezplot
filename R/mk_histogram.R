#' @title Create a function for making publishable ggplot2 histograms.
#'
#' @description
#' \code{mk_histogram} takes a data frame as input and returns a function for
#' making histograms of any continuous variable from the data frame. The output
#' function can also produce faceted histograms when supplied a categorical
#' variable, a facet_by variable, for grouping.
#'
#' @seealso \code{\link{get_summ_stats}}.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, facet_by = NULL, binw = NULL, bins = 30, facet_ncol = 1,
#'                font_size = 14, add_vline_median = TRUE, add_vline_mean = TRUE)}
#' \itemize{
#'      \item xvar     : string, name of a continuous variable for x-axis.
#'      \item facet_by : string, name of a categorical variable for grouping
#'                       x and creating facets. Default = NULL.
#'      \item binw     : width of the bins. Can be specified as a number, or
#'                       a function that calculates width from x. Default = NULL.
#'      \item bins     : number of bins. Overridden by binw. Default = 30. You
#'                       almost always want to change the defult. Andrew Gelman
#'                       recommends to choose a sufficiently large number to
#'                       show how raw data are distributed without any smoothing
#'                       as happens when estimating density. Read this blog post
#'                       for details:
#'                       http://andrewgelman.com/2009/10/23/variations_on_t/
#'      \item facet_ncol: number of columns when facetting. Default = 1.
#'                      Only works when facet_by is not NULL.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item add_vline_median: logical, if TRUE, add a vertical line at the
#'                              median of x. Default = TRUE.
#'      \item add_vline_mean: logical, if TRUE, add a vertical line at the mean
#'                            of x. Default = TRUE.
#' }
#' @export
#' @examples inst/examples/ex-mk_histogram.R
mk_histogram = function(df) {
        function(xvar, facet_by = NULL, binw = NULL, bins = 30, facet_ncol = 1,
                 font_size = 14, add_vline_median = TRUE, add_vline_mean = TRUE) {

                # --- Prep --- #

                # calc summary stats of x for each group as specified by the
                #       facet_by variable
                df_stats = get_summ_stats(df, xvar, facet_by)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, fill = "1")) +
                        geom_histogram(binwidth = binw, bins = bins, alpha =0.8)

                if (add_vline_median)
                        p = p + geom_vline(
                                        aes(xintercept = med, color = "median"),
                                        data = df_stats, size = 1,
                                        linetype = "dashed", show.legend = T
                                        )

                if (add_vline_mean)
                        p = p + geom_vline(
                                        aes(xintercept = avg, color = "mean"),
                                        data = df_stats, size = 1,
                                        linetype = "dashed", show.legend = T
                                        )

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + facet_wrap(vars(!!as.name(facet_by)),
                                           ncol = facet_ncol,
                                           scales = "free_x")


                # --- Format Legend --- #

                # add legend to indicate median and mean
                p = p + scale_color_manual(
                        name = "",
                        values = c(median = "#fc7d0b", mean = "#a3acb9"),
                        labels = c(median = paste("median:",
                                                  round(df_stats$med, 2)),
                                   mean = paste("mean:",
                                                round(df_stats$avg, 2)))
                        ) +
                        # but remove legend of the blue fill of the bars
                        guides(fill = FALSE)


                # --- Customize Theme --- #

                p + labs(x = xvar, y = "Frequency") + theme_cowplot(font_size)

        }
}

