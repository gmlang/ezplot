#' @title Create a function for making publishable ggplot2 horizontal barplots,
#' showing single or cumulative values of a continuous variable by 1 or 2
#' categorical variables.
#'
#' @description
#' \code{mk_barploth_resp} takes a data frame as input and returns a function
#' for making bar charts with any categorical variable from the data frame on
#' the y-axis and the value (or cumulative values) of any continuous variable on
#' the x-axis. The output function can also produce dodged bar charts when
#' supplied a second categorical variable, a fillby variable. The resulting
#' bar chart will have bars ordered ordered by the alphanumerical order
#' of the y levels by default, and labeled with the cumulative x values.
#' If the x values are decimals, user can specify the number of
#' decimals to display on the bar labels. If the x values are between 0 and 1,
#' user can choose to format the x-axis and the bar labels as percent (%). Plus,
#' the resulting plot will have a clean theme and clear fonts.
#' It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby = "1", yorder = "alphanumeric",
#'                show_pct = FALSE, label_decimals = 1, label_size = 3,
#'                legend_title = fillby, font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a continuous variable for x-axis.
#'      \item yvar     :  string, name of a categorical variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        subdividing and coloring the bars. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item yorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the y-axis.
#'                        Default = "alphanumeric".
#'      \item show_pct :  logical, if TRUE, format x-axis and bar labels as %;
#'                        otherwise, format them as comma. Default is FALSE.
#'      \item label_decimals: integer, the number of decimal points shown
#'                        on the bar labels. Default = 1.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'                        Hide bar labels when its value is 0.
#'      \item legend_title: string, legend title. Default is the name of the
#'                          fillby variable.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_barploth_resp.R
mk_barploth_resp = function(df) {
        function(xvar, yvar, fillby = "1", yorder = "alphanumeric",
                 show_pct = FALSE, label_decimals = 1, label_size = 3,
                 legend_title = fillby, font_size = 14) {

                # --- Prep --- #

                # --- order y levels
                #       We're doing things opposite here (for example,
                #       when asked to order them in descending order, we implement in
                #       ascending order) because we're drawing plot horizontally.
                # ---
                if (yorder == "alphanumeric")
                        df[[yvar]] = factor(df[[yvar]],
                                            sort(unique(df[[yvar]]),
                                                 decreasing = T))
                if (yorder == "descend")
                        df[[yvar]] = reorder(df[[yvar]], df[[xvar]],
                                             sum, na.rm = T)

                if (yorder == "ascend")
                        df[[yvar]] = reorder(df[[yvar]], -df[[xvar]],
                                             sum, na.rm = T)


                # get data frame of bar labels and positions
                df_label = get_bar_labels_resp(df, gp = yvar, resp = xvar,
                                               fillby = fillby)


                # --- Main Plot --- #

                p = ggplot(df, aes_string(y = yvar, weight = xvar,
                                          fill = fillby)) +
                        ggstance::geom_barh(position = "dodgev", alpha = 0.8)

                if (show_pct) {
                        # format continuous axis
                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent
                                                   )

                        # get bar label text
                        bar_labels = formattable::percent(df_label[[xvar]],
                                                          label_decimals)

                } else {
                        # format continuous axis
                        axis_breaks = pretty(c(0, df_label[[xvar]]), 10)
                        delta_size = axis_breaks[2] - axis_breaks[1]
                        all_bigger_than1 = all(setdiff(abs(axis_breaks), 0) > 1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(axis_breaks)
                        else con_axis_labs = axis_breaks
                        p = p + scale_x_continuous(
                                expand = c(0, 0),
                                limits = c(min(axis_breaks),
                                           max(axis_breaks) + delta_size),
                                breaks = axis_breaks,
                                labels = con_axis_labs
                                )

                        # get bar label text
                        bar_labels = scales::comma(
                                df_label[[xvar]],
                                accuracy = 1/10^label_decimals
                                )
                }

                # --- Format bar label --- #

                p = p + geom_text(aes(mid_pos, !!as.name(yvar)),
                                  data = df_label,
                                  label = bar_labels,
                                  size = label_size,
                                  position = ggstance::position_dodge2v(
                                          height = 0.9)
                                  )


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau(
                                "Color Blind", name = legend_title)
                }


                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_no_yaxis(font_size)

        }
}


