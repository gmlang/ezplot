#' @title Create a function for making publishable ggplot2 CDF plots.
#'
#' @description
#' \code{mk_cdfplot} takes a data frame as input and returns a function for
#' plotting empirical cumulative distribution function (ECDF) of any variable
#' (continuous or categorical) from the data frame.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, colorby = "1", complement = FALSE, linew = 0.7,
#'                add_vline_median = FALSE, show_label_median = add_vline_median,
#'                legend_title = colorby, legend_pos = "right",
#'                label_size = 3, font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item colorby. String, name of a categorical variable for grouping x.
#'      For each group, a CDF curve will be drawn with a different color.
#'      Default = "1", indicating no such variable is supplied.
#'      \item complement. Logical (default = FALSE), if TRUE, produce a function
#'      for plotting the complement of CDF.
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item add_vline_median. Logical (default = FALSE), if TRUE, add vertical
#'      dashed line segments at the median values going up and touching the
#'      CDF curves. To make it easy to look at, also add horizontal dashed line
#'      at y=0.5. Only effective when `complement = FALSE`.
#'      \item show_label_median. Logical (default is the same as the value of
#'      `add_vline_median`), if TRUE, show median values along the vertical
#'      median lines. Otherwise, don't show. Only effective when
#'      `complement = FALSE`.
#'      \item legend_title. String, legend title. Default is the name of the
#'      colorby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item label_size. Integer, size of bar label text. Default = 3.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#'      \item .... Optional parameters for `stat_ecfd()`. For example, instead
#'      of using the default `geom = 'step'`, you can set `geom = 'point'`,
#'      which will draw the ecdf in dots instead of stepped line segments. For
#'      another example, instead of using the default `pad = TRUE`, you can set
#'      `pad = FALSE` to stop the ecdf curve at its boundaries given by the data
#'      instead of extending it to +Inf and -Inf horizontally.
#' }
#' @export
#' @examples inst/examples/ex-mk_cdfplot.R
mk_cdfplot = function(df) {
        function(xvar, colorby = "1", complement = FALSE, linew = 0.7,
                 add_vline_median = FALSE, show_label_median = TRUE,
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

                if (complement) {
                        # extract data behind cdf plot
                        d = layer_data(p)

                        # ensures legend label of each curve displayed correctly
                        if (colorby != '1')
                                d$group = levels(as.factor(df[[colorby]]))[d$group]

                        # draw 1-cdf on y
                        p = ggplot(d, aes(x, 1-y, colour = group))

                        # add curves
                        if (!exists('geom')) {
                                p = p + geom_step(size = linew, alpha = 1)
                        } else {
                                if (geom == 'point') {
                                        p = p + geom_point(size = linew, alpha = 1)
                                } else {
                                        p = p + geom_step(size = linew, alpha = 1)
                                }
                        }
                }

                # break y-axis into 4 quarters from 0 to 1
                ybreaks = seq(0, 1, 0.25)
                p = p + scale_y_continuous(breaks = ybreaks, limits = range(ybreaks))

                # add vline at median when plotting CDF (not CCDF)
                if (!complement & add_vline_median) {
                        if (colorby == "1") {
                                med_xs = quantile(df[[xvar]], 0.5)
                        } else {
                                med_xs = sapply(
                                        split(df, df[[colorby]]),
                                        function(elt) quantile(elt[[xvar]], 0.5)
                                        )
                        }

                        # draw horizontal segment at y = 0.5
                        p = p + geom_segment(x = -Inf, xend = max(med_xs),
                                             y = 0.5, yend = 0.5,
                                             linetype = "dashed",
                                             color = cb_gray)

                        # draw vertical segment at the intersections of y=0.5
                        # and the curves
                        for (xi in round(med_xs, 2)) {
                                p = p + geom_segment(x = xi, xend = xi,
                                                     y = -Inf, yend = 0.5,
                                                     linetype = "dashed",
                                                     color = cb_gray)
                                if (show_label_median) {
                                        p = p + geom_text(
                                                x = xi, y = 0.05, label = xi,
                                                vjust = -0.3, nudge_y = 0.05,
                                                angle = 90, check_overlap = T,
                                                color = cb_gray, show.legend=F)
                                }
                        }
                }

                # --- Customize Theme --- #

                ylab = ifelse(complement, 'CCDF', 'CDF')
                p = p + labs(x = xvar, y = ylab) + theme_cowplot(font_size)


                # --- Format Legend --- #

                if (colorby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_color_tableau(
                                "Color Blind", name = legend_title) +
                                theme(legend.position = legend_pos)
                }

                p

        }
}
