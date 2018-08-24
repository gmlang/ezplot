#' @title Create a function for making publishable ggplot2 line plots.
#'
#' @description
#' \code{mk_lineplot} takes a data frame as input and returns a function for
#' making lineplots with any continuous variables from the data frame on the y
#' axis, and any continuous or categorical variables from the data frame on the
#' x axis. The x variable often measures time, for example, years. When supplied
#' a categorical fillby variable, the output function will produce lineplots
#' where lines are colored differently according to the levels of the fillby
#' variable.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, fillby = "1", pt_size = 1, linew = 0.7,
#'                font_size = 14, add_cnt_to_legend = TRUE,
#'                xlab = xvar, ylab = yvar, subtitle = NULL, ...)}
#' \itemize{
#'      \item xvar   : string, name of a continuous or categorical variable for
#'                     x-axis.
#'      \item yvar   : string, name of a continuous variable for y-axis.
#'      \item fillby : string, name of a categorical variable for
#'                     grouping and coloring the lines. Default = "1", meaning
#'                     no such variable is supplied.
#'      \item pt_size: number, size of the points. Default = 1.
#'      \item linew  : number, width of the line. Default = 0.7.
#'      \item font_size: overall font size. Default = 14. The font size of the
#'                       axes and legend text is a fraction of this value.
#'      \item add_cnt_to_legend: logical, when TRUE (default), it will show
#'                    the number of non-missing records for each level in the
#'                    fillby var.
#'      \item xlab   : string, the x-axis label. Default is xvar.
#'      \item ylab   : string, the y-axis label. Default is yvar.
#'      \item subtitle: string, subtitle of the plot. When NULL (default),
#'                      it'll show the number of records without any missings.
#'      \item ...    : other arguments for ggplot2::labs(), for example,
#'                     title, subtitle, caption and etc.
#' }
#'
#' @export
#' @examples
#' library(ezplot)
#'
#' # plot boxoffice/budget ratio over the years
#' plt = mk_lineplot(bo_bt_ratio_by_year)
#' title = "Boxoffice/Budget Ratio from 1913 to 2014"
#' p = plt("year", "bo_bt_ratio", xlab = NULL, ylab="boxoffice/budget ratio",
#'         title = title, caption = "data source: IMBD")
#' scale_axis(p, scale = "sqrt")
#'
#' # plot total budget and boxoffice over the years
#' plt = mk_lineplot(btbo_by_year)
#' title = "Annual Total Budget and Boxoffice from 1913 to 2014"
#' p = plt("year", "tot", fillby = "type", font_size = 10, xlab = NULL,
#'         ylab = "total amount ($billion)", title = title,
#'         add_cnt_to_legend = F)
#' scale_axis(p, scale = "log")
#'
#' library(dplyr)
#' plt = mk_lineplot(films %>% group_by(year_cat) %>%
#'                   summarise(avg_boxoffice = mean(boxoffice)))
#' plt("year_cat", "avg_boxoffice", xlab = NULL, ylab = "Mean Boxoffice",
#'     subtitle = "haha")
mk_lineplot = function(df) {
        function(xvar, yvar, fillby = "1", pt_size = 1, linew = 0.7,
                 font_size = 14, add_cnt_to_legend = T,
                 xlab = xvar, ylab = yvar, subtitle = NULL, ...) {

                # --- Prep  --- #

                if (is.null(subtitle)) {
                        # use number of non-NA rows as subtitle
                        tot_n = nrow(na.omit(df[c(xvar, yvar)]))
                        subtitle = paste("n =", tot_n)
                }


                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(xvar, yvar,
                                                            color = fillby)) +
                        ggplot2::geom_line(ggplot2::aes_string(group = fillby),
                                           size = linew, alpha = 0.8) +
                        ggplot2::geom_point(size = pt_size, alpha = 0.8)

                # break y-axis into 10 pieces from ymin to ymax
                ybreaks = pretty(df[[yvar]], n = 10)
                p = p + ggplot2::scale_y_continuous(
                                breaks = ybreaks,
                                limits = c(min(ybreaks), max(ybreaks))
                                )
                if (any(class(df[[xvar]]) %in% c("integer", "numeric"))) {
                        # break x-axis into 10 pieces from ymin to ymax
                        xbreaks = pretty(df[[xvar]], n = 10)
                        p = p + ggplot2::scale_x_continuous(
                                breaks = xbreaks,
                                limits = c(min(xbreaks), max(xbreaks))
                        )
                }

                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + ggplot2::guides(color = FALSE, fill = FALSE)
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

                p + ggplot2::labs(x = xlab, y = ylab, subtitle = subtitle, ...) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                aspect.ratio = 1,

                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                                )
        }
}
