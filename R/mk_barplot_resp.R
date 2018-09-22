#' @title Create a function for making publishable ggplot2 barplots, showing
#' single or cumulative values of a continuous variable by 1 or 2 categorical
#' variables.
#'
#' @description
#' \code{mk_barplot_resp} takes a data frame as input and returns a function for
#' making bar charts with any categorical variable from the data frame on the
#' x-axis and the value (or cumulative values) of any continuous variable on
#' the y-axis. The output function can also produce dodged bar charts when
#' supplied a second categorical variable, a fillby variable. The resulting
#' bar chart will have bars ordered ordered by the alphanumerical order
#' of the x levels by default, and labeled with the cumulative y values.
#' If the y values are decimals, user can specify the number of
#' decimals to display on the bar labels. If the y values are between 0 and 1,
#' user can choose to format the y-axis and the bar labels as percent (%). Plus,
#' the resulting plot will have a clean theme and clear fonts.
#' It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby = "1", xorder = "alphanumeric",
#'                show_pct = FALSE, label_decimals = 1, label_size = 3,
#'                font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        subdividing and coloring the bars. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item xorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the x-axis.
#'                        Default = "alphanumeric".
#'      \item show_pct :  logical, if TRUE, format y-axis and bar labels as %;
#'                        otherwise, format them as comma. Default is FALSE.
#'      \item label_decimals: integer, the number of decimal points shown
#'                        on the bar labels. Default = 1.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'                        Hide bar labels when its value is 0.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_barplot_resp.R
mk_barplot_resp = function(df) {
        function(xvar, yvar, fillby = "1", xorder = "alphanumeric",
                 show_pct = FALSE, label_decimals = 1, label_size = 3,
                 font_size = 14) {

                # --- Prep --- #

                if (xorder == "descend")
                        # reorder xvar levels in descending order of total y val
                        df[[xvar]] = reorder(df[[xvar]], df[[yvar]],
                                             function(y) -sum(y, na.rm = T))
                if (xorder == "ascend")
                        # reorder xvar levels in ascending order of total y val
                        df[[xvar]] = reorder(df[[xvar]], df[[yvar]],
                                             function(y) sum(y, na.rm = T))

                # get data frame of bar labels and positions
                df_label = get_bar_labels_resp(df, xvar, yvar, fillby)


                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, fill = fillby)) +
                        geom_bar(aes_string(weight = yvar),
                                 position = "dodge", alpha = 0.8)

                if (show_pct) {
                        # format continuous axis
                        p = p + scale_y_continuous(
                                limits = c(0, 1),
                                breaks = seq(0, 1, 0.1),
                                labels = scales::percent
                                )

                        # get bar label text
                        bar_labels = formattable::percent(df_label[[yvar]],
                                                          label_decimals)
                } else {
                        # format continuous axis
                        axis_breaks = pretty(c(0, df_label[[yvar]]), 10)
                        all_bigger_than1 = all(setdiff(abs(axis_breaks), 0) > 1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(axis_breaks)
                        else con_axis_labs = axis_breaks
                        p = p + scale_y_continuous(
                                limits = range(axis_breaks),
                                breaks = axis_breaks,
                                labels = con_axis_labs
                                )

                        # get bar label text
                        bar_labels = scales::comma(
                                df_label[[yvar]],
                                accuracy = ifelse(all_bigger_than1,
                                                  1, 1/10^label_decimals)
                                )
                }

                # --- Format bar label --- #

                p = p + geom_text(aes(!!as.name(xvar), !!as.name(yvar),
                                      label = bar_labels),
                                  data = df_label,
                                  vjust = -0.5, size = label_size,
                                  position = position_dodge(width = 0.9)
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


