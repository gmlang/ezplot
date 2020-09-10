#' @title Create a function for making ggplot2 horizontal bar charts, showing
#' the sums of a continuous variable by 1 or 2 categorical variables.
#'
#' @description
#' \code{mk_barploth_resp} takes a data frame as input and returns a function
#' for making bar charts with any categorical variable from the data frame on
#' the y-axis and the sums of any continuous variable on the x-axis. The output
#' function can also produce dodged bar charts when supplied a second categorical
#' variable. The resulting bar chart will have bars ordered by the alphanumerical
#' order of the y levels by default, and labeled by the sums of the x values.
#' Users can specify the number of decimal places to show on the bar labels. If
#' the sums of x values are between 0 and 1, users can choose to format the
#' x-axis and the bar labels as percent (%). Plus, the resulting figure has a
#' clean theme and font, and uses color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby = "1", yorder = "alphanumeric",
#'                show_pct = FALSE, label_decimals = 1, label_size = 3,
#'                legend_title = fillby, legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar String. Name of a continuous variable for x-axis.
#'      \item yvar String. Name of a categorical variable for y-axis.
#'      \item fillby String. Name of a different categorical variable for
#'      subdividing and coloring the bars. Default = "1", meaning no such
#'      variable is supplied.
#'      \item yorder String. Possible values: "alphanumeric" (default), "ascend"
#'      or "descend". It specifies how categories are ordered on the y-axis.
#'      \item show_pct Logical. If TRUE, format x-axis and bar labels as % and
#'      as comma otherwise. Default is FALSE.
#'      \item label_decimals Integer. The number of decimal places shown on
#'      the bar labels. Default = 1.
#'      \item label_size Integer. The size of bar label text. Default = 3. If 0,
#'      bar labels will not be shown.
#'      \item legend_title String. Legend title. Default is the name of the
#'      `fillby` variable.
#'      \item legend_pos String. Legend position. Possible values are
#'      "right" (default), 'left', 'top' and 'bottom'.
#'      \item font_size Number. Overall font size (default = 14). The font size
#'      of the axes and legend is a fraction of this value.
#' }
#'
#' @seealso \code{\link{get_bar_labels_resp}} for how to calculate the positions
#'          of the bar labels.
#' @export
#' @examples inst/examples/ex-mk_barploth_resp.R
mk_barploth_resp = function(df) {
        function(xvar, yvar, fillby = "1", yorder = "alphanumeric",
                 show_pct = FALSE, label_decimals = 1, label_size = 3,
                 legend_title = fillby, legend_pos = "right", font_size = 14) {

                # --- Prep --- #

                # order y levels:
                # We're doing things opposite here because we're drawing plot
                # horizontally. For example, when wanting to order them in
                # descending order, we implement in ascending order
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
                # print(df_label)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(y = yvar, weight = xvar))

                if (fillby == '1') {
                        p = p + ggstance::geom_barh(
                                position="dodgev", alpha=0.9, fill="#435474")
                } else {
                        p = p + ggstance::geom_barh(
                                position="dodgev", alpha=0.9, aes_string(fill=fillby))
                }

                if (show_pct) {
                        # format continuous axis
                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent)

                        # get bar label text
                        bar_labels = formattable::percent(
                                df_label[[xvar]], label_decimals)

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
                                labels = con_axis_labs)

                        # get bar label text
                        bar_labels = scales::comma(
                                df_label[[xvar]],
                                accuracy = 1/10^label_decimals)
                }

                # --- Format bar label --- #

                p = p + geom_text(aes(mid_pos, !!as.name(yvar)),
                                  data = df_label,
                                  label = bar_labels,
                                  size = label_size,
                                  position = ggstance::position_dodge2v(height=0.9),
                                  color = 'white')

                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = NULL) + theme_cowplot(font_size)


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
