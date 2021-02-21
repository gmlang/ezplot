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
#' \code{function(xvar, yvar, fillby = "1", yorder = NULL,
#'                is_x_pct = FALSE, show_pct = FALSE, label_decimals = 1,
#'                label_size = 3, legend_title = fillby, legend_pos = "right",
#'                font_size = 14)}
#' \itemize{
#'      \item xvar String. Name of a continuous variable for x-axis.
#'      \item yvar String. Name of a categorical variable for y-axis.
#'      \item fillby String. Name of a different categorical variable for
#'      subdividing and coloring the bars. Default = "1", meaning no such
#'      variable is supplied.
#'      \item yorder String. Possible values: NULL (default), "alphanumeric",
#'      "ascend" or "descend". It specifies how categories are ordered on the
#'      y-axis. When NULL, the categories are shown in their order in the data.
#'      \item is_x_pct. Logical, if TRUE, format x-axis as %. Default is FALSE.
#'      Set it to TRUE when the input xvar has values between 0 and 1 that can
#'      also be viewed as percents.
#'      \item show_pct. Logical, if TRUE, calculate the relative frequencies of
#'      the aggregated y values between each x category, and then display them
#'      as percentages on y-axis and also format bar labels as %; otherwise,
#'      format them as comma. Default is FALSE.
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
        function(xvar, yvar, fillby = "1", yorder = NULL,
                 is_x_pct = FALSE, show_pct = FALSE, label_decimals = 1,
                 label_size = 3, legend_title = fillby, legend_pos = "right",
                 font_size = 14) {

                # --- Prep --- #

                # order y levels:
                #    Doing things opposite here (for example, when asked to
                #    order them in descending order, we implement in ascending
                #    order) because we're drawing plot horizontally.
                if (is.null(yorder)) {
                        # order the y levels in their order in the data
                        lvls = rev(unique(df[[yvar]]))
                        df[[yvar]] = factor(df[[yvar]], levels = lvls)
                } else if (yorder == "alphanumeric") {
                        lvls = sort(unique(df[[yvar]]), decreasing=T)
                        df[[yvar]] = factor(df[[yvar]], levels = lvls)
                } else if (yorder == "descend") {
                        df[[yvar]] = reorder(df[[yvar]], df[[xvar]],
                                             sum, na.rm = T)
                } else if (yorder == "ascend") {
                        df[[yvar]] = reorder(df[[yvar]], -df[[xvar]],
                                             sum, na.rm = T)
                } else { } # do nothing


                # get data frame of bar labels and positions
                df_label = get_bar_labels_resp(df, yvar, xvar, fillby,
                                               is_x_pct, show_pct)
                pct_var = 'EZPLOT_pct'
                mid_var = 'EZPLOT_mid'

                # format bar label text
                bar_labels_pct = formattable::percent(
                        df_label[[pct_var]], label_decimals)
                bar_labels_raw = ifelse(
                        df_label[[xvar]] <= 1,
                        round(df_label[[xvar]], 2),
                        scales::comma(df_label[[xvar]], accuracy = 1))

                # --- Main Plot --- #

                p = ggplot(df, aes_string(y = yvar, weight = xvar, fill=fillby))

                if (is_x_pct) { # if x values are percents

                        # draw horizontal bars, where each bar has height equal
                        # to sum of pcts over a same y-level
                        p = p + ggstance::geom_barh(position = "dodgev",
                                                    alpha = 0.8) +
                                # label bars with pct in the middle
                                geom_text(aes_string(mid_var, yvar,
                                                     label = bar_labels_pct),
                                          data = df_label,
                                          size = label_size,
                                          position = ggstance::position_dodge2v(
                                                  height = 0.9, reverse = F))

                        # format x axis and make xlab
                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent)
                        xlab = xvar

                } else { # otherwise, there are two focuses:
                        # 1. relative pct of aggregated x-values at each y-level
                        # 2. sum of x-values at each y-level

                        if (show_pct) { # show relative percents on x-axis

                                if (fillby == "1") { # single bars show pct of each x category
                                        p = p + ggstance::geom_barh(
                                                aes(x = ..prop.., group = 1),
                                                alpha = 0.8) +
                                                geom_text(aes_string(pct_var, yvar),
                                                          data = df_label,
                                                          label = bar_labels_pct,
                                                          size = label_size,
                                                          hjust = -0.3) +
                                                geom_text(aes_string(mid_var, yvar),
                                                          data = df_label,
                                                          label = bar_labels_raw,
                                                          size = label_size)
                                } else { # stacked bars of pcts by fillby var
                                        p = p + ggstance::geom_barh(position = "fillv",
                                                                    alpha = 0.8) +
                                                geom_text(aes_string(pct_var, yvar,
                                                                     # must put label inside aes
                                                                     # to ensure correct label order
                                                                     label = bar_labels_pct),
                                                          data = df_label,
                                                          size = label_size,
                                                          position = ggstance::position_stackv(
                                                                  hjust = 0.5))
                                }

                                # format x-axis and make xlab
                                p = p + scale_x_continuous(expand = c(0, 0),
                                                           limits = c(0, 1),
                                                           breaks = seq(0, 1, 0.1),
                                                           labels = scales::percent)
                                xlab = paste('Percent of', xvar)

                        } else { # show aggregated raw values on x-axis

                                # single bars when fillby is '1' (default) and dodged
                                # bars when not (when there is a fillby var)
                                p = p + ggstance::geom_barh(position = "dodgev",
                                                            alpha = 0.8) +
                                        geom_text(aes_string(xvar, yvar),
                                                  data = df_label,
                                                  label = bar_labels_raw,
                                                  size = label_size,
                                                  hjust = -0.3,
                                                  position = ggstance::position_dodge2v(
                                                          height = 0.9, reverse = F)) +
                                        geom_text(aes_string(mid_var, yvar),
                                                  data = df_label,
                                                  label = bar_labels_pct,
                                                  size = label_size,
                                                  position = ggstance::position_dodge2v(
                                                          height = 0.9, reverse = F))

                                # format x-axis and make xlab
                                axis_breaks = pretty(c(0, df_label[[xvar]]), 10)
                                delta_size = axis_breaks[2] - axis_breaks[1]
                                p = p + scale_x_continuous(
                                        expand = c(0, 0),
                                        limits = c(min(axis_breaks),
                                                   max(axis_breaks) + delta_size),
                                        breaks = axis_breaks,
                                        labels = scales::comma)
                                xlab = xvar
                        }
                }

                # --- Customize Theme --- #

                p = p + labs(x = xlab, y = NULL) + theme_no_yaxis(font_size)

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
