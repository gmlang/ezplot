#' @title Create a function for making publishable ggplot2 line plots.
#'
#' @description
#' \code{mk_lineplot} takes a data frame as input and returns a function for
#' making lineplots with any continuous variables from the data frame on the y
#' axis, and any continuous or categorical variables from the data frame on the
#' x axis. The x variable often measures time, for example, years. When supplied
#' a categorical colorby variable, the output function will produce lineplots
#' where lines are colored differently according to the levels of the colorby
#' variable.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, colorby = "1", pt_size = 1, linew = 0.7,
#'                add_cnt_to_legend = TRUE, legend_title = colorby,
#'                legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous or categorical variable for x-axis.
#'      \item yvar. String, name of a continuous variable for y-axis.
#'      \item colorby. String, name of a categorical variable for grouping and
#'      coloring the lines. Default = "1", meaning no such variable is supplied.
#'      \item pt_size. Number, size of the points. Default = 1.
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item add_cnt_to_legend. Logical, when TRUE (default), it will show the
#'      number of non-missing records for each level in the colorby var.
#'      \item legend_title. String, legend title. Default uses the name of the
#'      colorby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_lineplot.R
mk_lineplot = function(df) {
        function(xvar, yvar, colorby = "1", pt_size = 1, linew = 0.7,
                 add_cnt_to_legend = TRUE, legend_title = colorby,
                 legend_pos = "right", font_size = 14) {

                # --- Prep  --- #

                # count total number of non-NA rows and use it as subtitle
                tot_n = nrow(na.omit(df[c(xvar, yvar)]))
                subtit = paste("n =", tot_n)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, yvar, color = colorby)) +
                        geom_line(aes_string(group = colorby),
                                  size = linew, alpha = 0.8) +
                        geom_point(size = pt_size, alpha = 0.8)

                # break y-axis into 10 pieces
                ybreaks = pretty(df[[yvar]], n = 10)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks))
                if (any(class(df[[xvar]]) %in% c("integer", "numeric"))) {
                        # break x-axis into 10 pieces
                        xbreaks = pretty(df[[xvar]], n = 10)
                        p = p + scale_x_continuous(
                                breaks = xbreaks,
                                limits = range(xbreaks)
                                )
                }


                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = yvar, subtitle = subtit) +
                        theme_cowplot(font_size)


                # --- Format Legend --- #

                if (colorby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else {
                        if (add_cnt_to_legend) {
                                # count number of non-NA observations for each
                                #   level of the colorby variable, and make
                                #   new legend label to include these counts
                                subdf = na.omit(df[c(xvar, yvar, colorby)])
                                tmp = dplyr::count(subdf, !!as.name(colorby))
                                legend_txt = paste(tmp[[colorby]],
                                                   paste0("(n = ", tmp$n, ")"))

                                # use colorblind-friendly colors and update
                                #       legend label
                                p = p + ggthemes::scale_color_tableau(
                                        "Color Blind", name = legend_title,
                                        labels = legend_txt)
                        } else {
                                # use colorblind-friendly colors
                                p = p + ggthemes::scale_color_tableau(
                                        "Color Blind", name = legend_title)
                        }
                        p = p + theme(legend.position = legend_pos)
                }

                p

        }
}
