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
#' @examples
#' library(dplyr)
#' library(ezplot)
#'
#' f = mk_boxplot(films)
#' f("year_cat", "rating", notched = T, font_size = 10)
#' f("year_cat", "rating", fillby = "made_money", notched = T)
#'
#' f = mk_boxplot(films %>% filter(year %in% 2010:2014))
#' f("year", "rating", notched = T) # throws error because "year" is integer
#' f = mk_boxplot(films %>% filter(year %in% 2010:2014) %>% mutate(year = factor(year)))
#' f("year", "rating", notched = T)
#'
#' f = mk_boxplot(ggplot2::mpg)
#' f("class", "hwy", fillby = "drv", font_size = 9) %>% add_labs(xlab = "class")
#'
#' f("year", "cty", fillby = "drv") # throws error because "year" is integer
#' mpg = ggplot2::mpg
#' mpg$year = as.character(mpg$year)
#' f = mk_boxplot(mpg)
#' f("year", "cty", fillby = "drv")
#'
#' df = data.frame(x = rep(c("A", "B"), 5),
#'                 y = c(56, 123, 546, 26, 62, 6, NaN, NA, NA, 15))
#' f = mk_boxplot(df)
#' f("x", "y") %>% add_labs(title = "Demo of title", subtitle = "demo of subtitle", caption = "fake data")
mk_boxplot = function(df) {
        function(xvar, yvar, fillby = "1", notched = FALSE, label_size = 3,
                 font_size = 14) {

                # --- Prep --- #

                if (any(class(df[[xvar]]) %in% c("integer", "numeric")))
                        stop(paste("The x variable,", paste0(xvar, ","),
                                   "is integer or numeric. Change to factor or character."))

                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(xvar, yvar)) +
                        ggplot2::geom_boxplot(
                                ggplot2::aes_string(fill = fillby),
                                alpha = 0.8, notch = notched)

                # draw mean as spade shape
                p = p + ggplot2::stat_summary(
                                ggplot2::aes_string(group = fillby),
                                fun.y = mean,
                                geom = "point", size = 1, shape = 5,
                                position = ggplot2::position_dodge(0.75)
                                )

                # show number of observations above ymax
                #       (drops NAs and show correct counts automatically)
                get_n = function(x) data.frame(y = max(x),
                                               label = paste("n =", length(x)))
                p = p + ggplot2::stat_summary(
                                ggplot2::aes_string(group = fillby),
                                fun.data = get_n, geom = "text",
                                size = label_size, vjust = -0.8,
                                position = ggplot2::position_dodge(0.75)
                                )

                # break y-axis into 10 pieces from ymin to ymax
                ybreaks = pretty(df[[yvar]], n = 10)
                p = p + ggplot2::scale_y_continuous(
                                breaks = ybreaks,
                                limits = c(min(ybreaks), max(ybreaks))
                                )

                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + ggplot2::guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + ggplot2::labs(x = NULL, y = yvar) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                        )

        }
}
