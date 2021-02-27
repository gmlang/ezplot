#' @title Create a function for making publishable ggplot2 dotplots.
#'
#' @description
#' \code{mk_dotplot} takes a data frame as input and returns a function for
#' making dotplots with any continuous variable from the data frame on the
#' x-axis. The dots are stacked along the y axis. The returned faction can also
#' optionally take a grouping variable so that the dots are filled with
#' different colors for different groups.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, fillby = "1",
#'                legend_title = fillby, legend_pos = "right",
#'                label_size = 3, font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item fillby. String, name of a categorical variable for grouping and
#'      coloring each dot. Default = "1", meaning no such variable is supplied.
#'      \item legend_title. String, legend title. Default is the name of the
#'      fillby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item label_size. Integer, size of bar label text. Default = 3.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#'      \item .... Other parameters that can be passed into `ggplot2::geom_dotplot()`.
#'      For example, `binwidth`, which can be specified as a number or a
#'      function that calculates width from x. Default is 30 bins, which can
#'      be overridden by the supplied `binwidth` value. Two other common parameters
#'      are `dotsize` and `stackratio`, which controls the size of the dots
#'      and the spaces between the dots.
#' }
#' @export
#' @examples inst/examples/ex-mk_dotplot.R
mk_dotplot = function(df) {
        function(xvar, fillby = "1", legend_title = fillby,
                 legend_pos = "right", label_size = 3, font_size = 14, ...) {

                # --- Prep --- #

                if (fillby != '1' & class(df[[fillby]]) != 'factor') {
                        # must convert the fillby var to factor
                        df[[fillby]] = factor(df[[fillby]])
                }

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, fill = fillby))
                # break x-axis into 10 ticks from xmin to xmax
                p = scale_axis(p, axis = 'x') # place before geom_dotplot() to
                # avoid showing the message of 'bins = 30' twice
                p = p + geom_dotplot(alpha = 0.8, stackgroups = TRUE,
                                     binpositions = 'all', ...) +
                        scale_y_continuous(NULL, breaks = NULL)

                # --- Customize Theme --- #

                p = p + labs(x = xvar) + theme_cowplot(font_size)


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau(
                                "Color Blind", name = legend_title) +
                                theme(legend.position = legend_pos)
                }

                p

        }
}
