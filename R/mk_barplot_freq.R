#' @title Create a function for making publishable ggplot2 barplots, showing
#' frequency counts (or percentages) of a categorical variable or 2 categorical
#' variables.
#'
#' @description
#' \code{mk_barplot_freq} takes a data frame as input and returns a function for
#' making bar charts with any categorical variable from the data frame on the
#' x-axis and frequency counts (or percents) of its levels on the y-axis.
#' The output function can also produce dodged (for counts) or stacked
#' (for percents) bar charts when supplied a second categorical variable, a
#' fillby variable. The resulting bar chart will have bars ordered by the
#' alphanumerical order of the x levels and labeled with both counts and
#' percents by default. Plus, it'll have a clean theme and clean fonts.
#' It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, fillby = "1", xorder = NULL, show_pct = FALSE,
#'                label_decimals = 1, label_size = 3, legend_title = fillby,
#'                legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a categorical variable for x-axis.
#'      \item fillby. String, name of a different categorical variable for
#'      subdividing and coloring the bars. Default is "1", meaning no such
#'      variable is supplied.
#'      \item xorder. String, Possible values: NULL (default), "alphanumeric",
#'      "ascend" or "descend". It specifies how categories are ordered on the
#'      x-axis. When NULL, the categories are shown in their order in the data.
#'      \item show_pct. logical, if TRUE, show percent on y-axis; otherwise,
#'      show count. Default is FALSE.
#'      \item label_decimals. Integer, the number of decimal points shown on the
#'      bar labels. Default = 1.
#'      \item label_size. Integer, size of bar label text. Default is 3. Hide
#'      bar labels when its value is 0.
#'      \item legend_title. String, legend title. Default is the name of the
#'      fillby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_barplot_freq.R
mk_barplot_freq = function(df) {
        function(xvar, fillby = "1", xorder = NULL, show_pct = FALSE,
                 label_decimals = 1, label_size = 3, legend_title = fillby,
                 legend_pos = "right", font_size = 14) {

                # --- Prep --- #

                if (is.null(xorder)) {
                        # order the x levels in their order in the data
                        if (is.character(df[[xvar]])) {
                                lvls = unique(df[[xvar]])
                                df[[xvar]] = factor(df[[xvar]], levels = lvls)
                        }
                } else if (xorder == "alphanumeric") {
                        lvls = sort(unique(df[[xvar]]))
                        df[[xvar]] = factor(df[[xvar]], levels = lvls)
                } else if (xorder == "descend") {
                        # reorder x levels in descending order of their counts
                        df[[xvar]] = reorder(df[[xvar]], df[[xvar]],
                                             function(x) -length(x))
                } else if (xorder == "ascend") {
                        # reorder x levels in ascending order of their counts
                        df[[xvar]] = reorder(df[[xvar]], df[[xvar]],
                                             function(x) length(x))
                } else {
                        # do nothing
                }

                # get data frame of bar labels and positions
                df_label = get_bar_labels_freq(df, xvar, fillby, show_pct)
                pct_var = 'EZPLOT_pct'
                cnt_var = 'EZPLOT_cnt'
                mid_var = 'EZPLOT_mid'

                # format bar label text
                bar_labels_pct = formattable::percent(
                        df_label[[pct_var]], label_decimals)
                bar_labels_cnt = scales::comma(
                        df_label[[cnt_var]], accuracy = 1)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, fill = fillby))

                if (show_pct) { # show percent instead of counts on y-axis

                        if (fillby == "1") {
                                # single bar show pct of each x catgeory
                                p = p + geom_bar(aes(y = ..prop.., group = 1),
                                                 alpha = 0.8) +
                                        geom_text(aes_string(xvar, pct_var),
                                                  data = df_label,
                                                  label = bar_labels_pct,
                                                  size = label_size,
                                                  vjust = -0.5) +
                                        geom_text(aes_string(xvar, mid_var),
                                                  data = df_label,
                                                  label = bar_labels_cnt,
                                                  size = label_size)
                        } else {
                                # stacked bars of pcts by fillby var
                                p = p + geom_bar(position = "fill", alpha=0.8) +
                                        geom_text(aes_string(xvar, pct_var),
                                                  data = df_label,
                                                  label = bar_labels_pct,
                                                  size = label_size,
                                                  position = position_stack(
                                                          vjust = 0.5))
                        }

                        # format y-axis and make ylab
                        p = p + scale_y_continuous(limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent)
                        ylab = "Relative Frequency (%)"

                } else { # show count on y-axis, and this is default
                        p = p + geom_bar(position = "dodge", alpha = 0.8) +
                                geom_text(aes_string(xvar, cnt_var),
                                          data = df_label,
                                          label = bar_labels_cnt,
                                          size = label_size,
                                          vjust = -0.5,
                                          position = position_dodge(width=0.9)) +
                                geom_text(aes_string(xvar, mid_var),
                                          data = df_label,
                                          label = bar_labels_pct,
                                          size = label_size,
                                          position = position_dodge(width=0.9))

                        # format y-axis and make ylab
                        axis_breaks = pretty(c(0, df_label[[cnt_var]]), 10)
                        p = p + scale_y_continuous(limits = range(axis_breaks),
                                                   breaks = axis_breaks,
                                                   labels = scales::comma)
                        ylab = "Frequency"
                }


                # --- Customize Theme --- #

                p = p + labs(x = NULL, y = ylab) + theme_cowplot(font_size)


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = 'none', fill = 'none')
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau(
                                "Color Blind", name = legend_title) +
                                theme(legend.position = legend_pos)
                }

                p
        }
}


