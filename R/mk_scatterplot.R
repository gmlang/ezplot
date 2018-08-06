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
#'                jitter = FALSE, font_size = 14, xlab = xvar, ylab = yvar,
#'                ...)}
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
#'      \item xlab     :  string, the x-axis label. Default is xvar.
#'      \item ylab     :  string, the y-axis label. Default is yvar.
#'      \item ...      :  other arguments for ggplot2::labs(), for example,
#'                        title, subtitle, caption and etc.
#' }
#'
#' @export
#' @examples
#' library(ezplot)
#' plt = mk_scatterplot(films)
#'
#' plt("budget", "boxoffice")
#' plt("budget", "boxoffice", fillby = "year_cat",  alpha = 0.2,
#'     title = "Boxoffice and Budget are related, try log scales",
#'     caption = "Source: IMDB")
#' plt("year", "rating", font_size = 10, xlab = NULL)
#' plt("year", "rating", jitter = T, xlab = NULL)
#'
#' df = data.frame(V1 = c(56, 123, 546, 26, 62, 6, NaN, NA, NA, 15),
#'                 V2 = c(21, 231, 5, 5, 32, NA, 1, 231, 5, 200),
#'                 V3 = c(NA, NA, 24, 51, 53, 231, NA, 153, 6, 700),
#'                 V4 = c(2, 10, NA, 20, 56, 1, 1, 53, 40, 5000)
#'                 )
#' nrow(df)
#'
#' plt = mk_scatterplot(df)
#' plt("V1", "V2")
#' plt("V1", "V3")
mk_scatterplot = function(df) {
        function(xvar, yvar, fillby = "1", alpha = 0.8, pt_size = 1,
                 jitter = FALSE, font_size = 14, xlab = xvar, ylab = yvar,
                 ...) {

                # --- Prep  --- #

                # set subtitle
                tot_n = nrow(na.omit(df[c(xvar, yvar)]))
                subtit = paste("n =", tot_n)


                # --- Main Plot --- #

                p = ggplot2::ggplot(
                        df, ggplot2::aes_string(xvar, yvar, color = fillby))

                if (jitter) p = p + ggplot2::geom_jitter(
                        alpha = alpha, size = pt_size, shape = 16)
                else p = p + ggplot2::geom_point(
                        alpha = alpha, size = pt_size, shape = 16)

                # break y-axis into 10 pieces from ymin to ymax;
                # break x-axis into 10 pieces from xmin to xmax
                ybreaks = pretty(df[[yvar]], n = 10)
                xbreaks = pretty(df[[xvar]], n = 10)
                p = p + ggplot2::scale_y_continuous(
                                breaks = ybreaks,
                                limits = c(min(ybreaks), max(ybreaks))
                                ) +
                        ggplot2::scale_x_continuous(
                                breaks = xbreaks,
                                limits = c(min(xbreaks), max(xbreaks))
                                )

                # --- Format Legend --- #

                if (fillby == 1) { # remove legend
                        p = p + ggplot2::guides(color = FALSE, fill = FALSE)
                } else {
                        # count number of non-NA observations for each level of
                        #       the fillby variable, and make new legend label
                        #       to include those counts
                        subdf = na.omit(df[c(xvar, yvar, fillby)])
                        tmp = dplyr::count(subdf, !!as.name(fillby))
                        legend_txt = paste(tmp[[fillby]],
                                           paste0("(n = ", tmp$n, ")"))

                        # use colorblind-friendly colors and update legend label
                        p = p + ggthemes::scale_color_tableau(
                                "Color Blind", labels = legend_txt)
                }


                # --- Customize Theme --- #

                p + ggplot2::labs(x = xlab, y = ylab, subtitle = subtit, ...) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                aspect.ratio = 1,

                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                                )
        }
}
