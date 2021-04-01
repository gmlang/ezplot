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
#' \code{function(xvar, yvar, fillby = "1", xorder = NULL,
#'                show_pct = FALSE, label_decimals = 1, label_size = 3,
#'                legend_title = fillby, legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a categorical variable for x-axis.
#'      \item yvar. String, name of a continuous variable for y-axis.
#'      \item fillby. String, name of a different categorical variable for
#'      subdividing and coloring the bars. Default = "1", meaning no such
#'      variable is supplied.
#'      \item xorder. String, Possible values: NULL (default), "alphanumeric",
#'      "ascend" or "descend". It specifies how categories are ordered on the
#'      x-axis. When NULL, the categories are shown in their order in the data.
#'      \item is_y_pct. Logical, if TRUE, format y-axis as %. Default is FALSE.
#'      Set it to TRUE when the input yvar has values between 0 and 1 that can
#'      also be viewed as percents.
#'      \item show_pct. Logical, if TRUE, calculate the relative frequencies of
#'      the aggregated y values between each x category, and then display them
#'      as percentages on y-axis and also format bar labels as %; otherwise,
#'      format them as comma. Default is FALSE.
#'      \item label_decimals. Integer, the number of decimal points shown on the
#'      bar labels. Default = 1.
#'      \item label_size. Integer, size of bar label text. Default = 3. Hide bar
#'      labels when its value is 0.
#'      \item legend_title. String, legend title. Default is the name of the
#'      fillby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_barplot_resp.R
mk_barplot_resp = function(df) {
        function(xvar, yvar, fillby = "1", xorder = NULL, is_y_pct = FALSE,
                 show_pct = FALSE, label_decimals = 1, label_size = 3,
                 legend_title = fillby, legend_pos = "right", font_size = 14) {

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
                        # reorder x levels in descending order of total y
                        df[[xvar]] = reorder(df[[xvar]], -df[[yvar]],
                                             sum, na.rm = T)
                } else if (xorder == "ascend") {
                        # reorder x levels in ascending order of total y
                        df[[xvar]] = reorder(df[[xvar]], df[[yvar]],
                                             sum, na.rm = T)
                } else {
                        # do nothing
                }

                # get data frame of bar labels and positions
                df_label = get_bar_labels_resp(df, xvar, yvar, fillby,
                                               is_y_pct, show_pct)
                pct_var = 'EZPLOT_pct'
                mid_var = 'EZPLOT_mid'

                # format bar label text
                bar_labels_pct = formattable::percent(
                        df_label[[pct_var]], label_decimals)
                bar_labels_raw = ifelse(
                        df_label[[yvar]] <= 1,
                        round(df_label[[yvar]], 2),
                        scales::comma(df_label[[yvar]], accuracy = 1))

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, weight = yvar, fill = fillby))

                if (is_y_pct) { # if y values are percents

                        # draw vertical bars, where each bar has height equal
                        # to sum of pcts over a same x-level
                        p = p + geom_bar(position = "dodge", alpha = 0.8) +
                                # label bars with pct at the top
                                geom_text(aes_string(xvar, yvar,
                                                     label = bar_labels_pct),
                                          data = df_label,
                                          size = label_size,
                                          vjust = -0.5,
                                          position = position_dodge2(width=0.9))

                        # format y axis and make ylab
                        p = p + scale_y_continuous(limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent)
                        ylab = yvar

                } else { # otherwise, there are two focuses:
                        # 1. relative pct of aggregated y-values at each x-level
                        # 2. sum of y-values at each x-level

                        if (show_pct) { # show relative percents on y-axis
                                if (fillby == "1") { # single bars
                                        p = p + geom_bar(aes(y = ..prop.., group = 1),
                                                         alpha = 0.8) +
                                                # label pct at the end of each bar
                                                geom_text(aes_string(xvar, pct_var),
                                                          data = df_label,
                                                          label = bar_labels_pct,
                                                          size = label_size,
                                                          vjust = -0.5) +
                                                # label raw at the middle of each bar
                                                geom_text(aes_string(xvar, mid_var),
                                                          data = df_label,
                                                          label = bar_labels_raw,
                                                          size = label_size)
                                } else { # stacked bars colored by the fillby var
                                        p = p + geom_bar(position="fill", alpha=0.8) +
                                                geom_text(aes_string(xvar, pct_var),
                                                          data = df_label,
                                                          label = bar_labels_pct,
                                                          size = label_size,
                                                          position = position_stack(
                                                                  vjust=0.5))
                                }

                                # format y-axis and make y-axis label
                                p = p + scale_y_continuous(limits = c(0, 1),
                                                           breaks = seq(0, 1, 0.1),
                                                           labels = scales::percent)
                                ylab = paste('Percent of', yvar)

                        } else { # show aggregated raw values on y-axis

                                # single bars when fillby is '1' (default) and dodged
                                # bars when not (when there is a fillby var)
                                p = p + geom_bar(position="dodge", alpha=0.8) +
                                        # show raw at the end of each bar
                                        geom_text(aes_string(xvar, yvar),
                                                  data = df_label,
                                                  label = bar_labels_raw,
                                                  size = label_size,
                                                  vjust = -0.5,
                                                  position = position_dodge(width=0.9)) +
                                        # show pct in the middle of each bar
                                        geom_text(aes_string(xvar, mid_var),
                                                  data = df_label,
                                                  label = bar_labels_pct,
                                                  size = label_size,
                                                  position = position_dodge(width=0.9))

                                # format y-axis and make y-axis label
                                axis_breaks = pretty(c(0, df_label[[yvar]]), 10)
                                p = p + scale_y_continuous(limits = range(axis_breaks),
                                                           breaks = axis_breaks,
                                                           labels = scales::comma)
                                ylab = yvar
                        }

                }

                # --- Customize Theme --- #

                p = p + labs(x = NULL, y = ylab) + theme_cowplot(font_size)


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


