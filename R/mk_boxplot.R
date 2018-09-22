#' @title Create a function for making publishable ggplot2 boxplots.
#'
#' @description
#' \code{mk_boxplot} takes a data frame as input and returns a function for
#' making boxplots with any categorical variable from the data frame on the
#' x-axis and any continuous variable on the y-axis. The output function can
#' also produce dodged boxplots when supplied a second categorical variable, a
#' fillby variable.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, fillby = "1", notched = FALSE, label_size = 3,
#'                font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        breaking down the y values of each box. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item notched  :  logical, draw notched boxplots when TRUE;
#'                        otherwise, draw regular boxplots. Default = FALSE.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_boxplot.R
mk_boxplot = function(df) {
        function(xvar, yvar, fillby = "1", notched = FALSE, label_size = 3,
                 font_size = 14) {

                # --- Prep --- #

                if (any(class(df[[xvar]]) %in% c("integer", "numeric")))
                        stop(paste("The x variable,", paste0(xvar, ","),
                                   "is integer or numeric. Change to factor or character.")
                             )

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, yvar)) +
                        geom_boxplot(aes_string(fill = fillby),
                                     alpha = 0.8, notch = notched)

                # draw mean as spade shape
                p = p + stat_summary(aes_string(group = fillby),
                                     fun.y = mean, geom = "point", size = 1,
                                     shape = 5, position = position_dodge(0.75)
                                     )

                # show number of observations above ymax
                #       (drops NAs and show correct counts automatically)
                get_n = function(x) data.frame(y = max(x),
                                               label = paste("n =", length(x))
                                               )
                p = p + stat_summary(aes_string(group = fillby),
                                     fun.data = get_n, geom = "text",
                                     size = label_size, vjust = -0.8,
                                     position = position_dodge(0.75)
                                     )

                # break y-axis into 10 pieces from ymin to ymax
                ybreaks = pretty(df[[yvar]], n = 10)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks)
                                           )

                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + labs(x = NULL, y = yvar) + theme_cowplot(font_size)

        }
}
