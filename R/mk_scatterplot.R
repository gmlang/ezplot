#' @title Create a function for making publishable ggplot2 scatterplots.
#'
#' @description
#' \code{mk_scatterplot} takes a data frame as input and returns a function for
#' making scatterplots with any continuous variables from the data frame on the
#' x and y axes. When supplied a categorical fillby variable, the output
#' function will produce scatterplots where the points are colored differently
#' according to the levels of the fillby variable.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, fillby = "1", alpha = 0.8, pt_size = 1,
#'                jitter = FALSE, font_size = 14, add_cnt_to_legend = T)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        grouping and coloring the points. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item alpha    :  a number between 0 and 1, transparency level of the
#'                        point colors. Smaller value means more transparent.
#'                        Default = 0.8.
#'      \item pt_size  :  number, size of the points. Default = 1.
#'      \item jitter   :  logical, jitter points if TRUE. Used when there're
#'                        overlapping points. Default = FALSE.
#'      \item font_size:  overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item add_cnt_to_legend: logical, when TRUE (default), it will show
#'                    the number of non-missing records for each level in the
#'                    fillby var.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_scatterplot.R
mk_scatterplot = function(df) {
        function(xvar, yvar, fillby = "1", alpha = 0.8, pt_size = 1,
                 jitter = FALSE, font_size = 14, add_cnt_to_legend = T) {

                # --- Prep  --- #

                # count total number of non-NA rows and use it as subtitle
                tot_n = nrow(na.omit(df[c(xvar, yvar)]))
                subtit = paste("n =", tot_n)


                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, yvar, color = fillby))

                if (jitter) p = p + geom_jitter(
                        alpha = alpha, size = pt_size, shape = 16)
                else p = p + geom_point(
                        alpha = alpha, size = pt_size, shape = 16)

                # break y-axis into 10 pieces from ymin to ymax;
                # break x-axis into 10 pieces from xmin to xmax
                ybreaks = pretty(df[[yvar]], n = 10)
                xbreaks = pretty(df[[xvar]], n = 10)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks)) +
                        scale_x_continuous(breaks = xbreaks,
                                           limits = range(xbreaks))

                # --- Format Legend --- #

                if (fillby == 1) { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else {
                        if (add_cnt_to_legend) {
                                # count number of non-NA observations for each
                                #   level of the fillby variable, and make
                                #   new legend label to include these counts
                                subdf = na.omit(df[c(xvar, yvar, fillby)])
                                tmp = dplyr::count(subdf, !!as.name(fillby))
                                legend_txt = paste(tmp[[fillby]],
                                                   paste0("(n = ", tmp$n, ")"))

                                # use colorblind-friendly colors and update
                                #       legend label
                                p = p + ggthemes::scale_color_tableau(
                                        "Color Blind", labels = legend_txt)
                        } else {
                                # use colorblind-friendly colors
                                p = p + ggthemes::scale_color_tableau(
                                        "Color Blind")
                        }

                }


                # --- Customize Theme --- #

                p + labs(x = xvar, y = yvar, subtitle = subtit) +
                        theme_cowplot(font_size)


        }
}
