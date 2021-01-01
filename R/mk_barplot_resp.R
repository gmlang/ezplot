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
#'                legend_title = fillby, legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a categorical variable for x-axis.
#'      \item yvar. String, name of a continuous variable for y-axis.
#'      \item fillby. String, name of a different categorical variable for
#'      subdividing and coloring the bars. Default = "1", meaning no such
#'      variable is supplied.
#'      \item xorder. String, "alphanumeric", "ascend" or "descend". It specifies
#'      how categories are ordered on the x-axis. Default = "alphanumeric".
#'      \item show_pct. Logical, if TRUE, format y-axis and bar labels as %;
#'      otherwise, format them as comma. Default is FALSE.
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
        function(xvar, yvar, fillby = "1", xorder = "alphanumeric",
                 show_pct = FALSE, label_decimals = 1, label_size = 3,
                 legend_title = fillby, legend_pos = "right", font_size = 14) {

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
                df_label = get_bar_labels_resp(df, xvar, yvar, fillby, show_pct)


                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, weight = yvar, fill = fillby))

                if (show_pct) { # show percent instead of raw values on y-axis

                        if (fillby == "1") { # single bars
                                p = p + geom_bar(aes(y = ..prop.., group = 1), alpha = 0.8) +
                                        geom_text(aes(!!as.name(xvar), pct,
                                                      label = formattable::percent(pct, label_decimals)),
                                                  data = df_label, vjust = -0.5,
                                                  size = label_size) +
                                        geom_text(aes(!!as.name(xvar), mid_pos,
                                                      label = ifelse(!!as.name(yvar) <= 1,
                                                                     round(!!as.name(yvar), 2),
                                                                     scales::comma(!!as.name(yvar), accuracy = 1))
                                                                     ),
                                                  data = df_label,
                                                  size = label_size)
                        } else { # stacked bars colored by the fillby var
                                p = p + geom_bar(position = "fill", alpha = 0.8) +
                                        geom_text(aes(!!as.name(xvar), pct,
                                                      label = formattable::percent(pct, label_decimals)),
                                                  data = df_label,
                                                  size = label_size,
                                                  position = position_stack(vjust = 0.5))
                        }

                        p = p + scale_y_continuous(limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent)
                        ylab = paste('Percent of', yvar)

                } else { # show raw value on y-axis

                        # single bars when fillby is '1' (default) and dodged
                        # bars when not (when there is a fillby var)
                        p = p + geom_bar(position = "dodge", alpha = 0.8) +
                                geom_text(aes(!!as.name(xvar), !!as.name(yvar),
                                              label = ifelse(!!as.name(yvar) <= 1,
                                                             round(!!as.name(yvar), 2),
                                                             scales::comma(!!as.name(yvar), accuracy = 1))
                                              ),
                                          data = df_label, vjust = -0.5,
                                          size = label_size,
                                          position = position_dodge(width = 0.9)) +
                                geom_text(aes(!!as.name(xvar), mid_pos,
                                              label = formattable::percent(pct, label_decimals)),
                                          data = df_label, size = label_size,
                                          position = position_dodge(width = 0.9))

                        axis_breaks = pretty(c(0, df_label[[yvar]]), 10)
                        p = p + scale_y_continuous(limits = range(axis_breaks),
                                                   breaks = axis_breaks,
                                                   labels = scales::comma)
                        ylab = yvar

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


