#' @title Create a function for making publishable ggplot2 CDF plots.
#'
#' @description
#' \code{mk_cdfplot} takes a data frame as input and returns a function for
#' plotting empirical cumulative distribution function (ECDF) of any variable
#' (continuous or categorical) from the data frame.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, colorby = "1", linew = 0.7, add_hline_median = FALSE,
#'                legend_title = colorby, legend_pos = "right",
#'                label_size = 3, font_size = 14)}
#' \itemize{
#'      \item xvar     : string, name of a continuous variable for x-axis.
#'      \item colorby  : string, name of a categorical variable for grouping x.
#'                       For each group, a CDF curve will be drawn with a
#'                       different color. Default = "1", indicating no such
#'                       variable is supplied.
#'      \item linew    : number, width of the line. Default = 0.7.
#'      \item add_hline_median: logical (default = FALSE), if TRUE, add a
#'                horizontal dashed line at the median ECDF value on the y-axis
#'                and a vertical dashed line at the corresponding x value.
#'      \item legend_title: string, legend title. Default is the name of the
#'                          colorby variable.
#'      \item legend_pos:   string, legend position. Default = "right".
#'      \item label_size: integer, size of bar label text. Default = 3.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item ...: optional parameters for `stat_ecfd()`. For example, instead of
#'                 using the default `geom = 'step'`, you can set `geom = 'point'`,
#'                 which will draw the ecdf in dots instead of stepped line
#'                 segments. For another example, instead of using the default \
#'                 `pad = TRUE`, you can set `pad = FALSE` to stop the ecdf curve
#'                 at its boundaries given by the data instead of extending it
#'                 to +Inf and -Inf horizontally.
#' }
#' @export
#' @examples inst/examples/ex-mk_cdfplot.R
mk_cdfplot = function(df) {
        function(xvar, colorby = "1", linew = 0.7, add_hline_median = FALSE,
                 legend_title = colorby, legend_pos = "right", label_size = 3,
                 font_size = 14, ...) {

                # --- Prep --- #

                if (class(df[[xvar]]) == "character")
                        stop(paste("The x variable", xvar, "is character type.",
                                    "Please supply a numeric/integer variable."))
                if (class(df[[xvar]]) == "factor")
                        stop(paste("The x variable", xvar, "is factor type.",
                                   "Please supply a numeric/integer variable."))


                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, color = colorby)) +
                        stat_ecdf(size = linew, alpha = 1, ...)

                # break y-axis into 10 pieces from ymin to ymax
                ybreaks = seq(0, 1, 0.25)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks))


                # add lines at the median and its corresponding x value
                if (add_hline_median) {
                        if (colorby == "1") {
                                med_xs = quantile(df[[xvar]], 0.5)
                        } else {
                                med_xs = sapply(
                                        split(df, df[[colorby]]),
                                        function(elt) quantile(elt[[xvar]], 0.5)
                                        )
                        }

                        # p = p + geom_hline(yintercept = 0.5, size = linew,
                        #                    linetype = "dashed", alpha = 0.4) +
                        #         geom_vline(xintercept = med_xs, size = linew,
                        #                    linetype = "dashed", alpha = 0.4)

                        # draw horizontal segment at y = 0.5
                        p = p + geom_segment(x = -Inf, xend = max(med_xs),
                                             y = 0.5, yend = 0.5,
                                             alpha = 0.05,
                                             linetype = "dashed")

                        # draw vertical segment at the intersections of y=0.5
                        # and the curves
                        for (xi in round(med_xs, 2))
                                p = p + geom_segment(x = xi, xend = xi,
                                                     y = -Inf, yend = 0.5,
                                                     alpha = 0.05,
                                                     linetype = "dashed") +
                                geom_text(x = xi, y = 0.05, label = xi,
                                          vjust = -0.3, nudge_y = 0.05,
                                          angle = 90, check_overlap = T,
                                          color = 'darkgray', show.legend = F)
                }

                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = 'CDF') + theme_cowplot(font_size)


                # --- Format Legend --- #

                if (colorby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau(
                                "Color Blind", name = legend_title) +
                                theme(legend.position = legend_pos)
                }

                p

        }
}
