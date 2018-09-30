#' @title Create a function for making publishable ggplot2 horizontal barplots,
#' showing frequency counts (or percentages) of a categorical variable or 2
#' categorical variables.
#'
#' @description
#' \code{mk_barploth_freq} takes a data frame as input and returns a function
#' for making horizontal bar charts with any categorical variable from the data
#' frame on the y-axis and frequency counts (or percents) of its levels on the
#' x-axis. The output function can also produce dodged (for counts) or stacked
#' (for percents) bar charts when supplied a second categorical variable, a
#' fillby variable. The resulting bar chart will have bars ordered by the
#' alphanumerical order of the y levels by default, and bars
#' labeled with both counts and percents. Plus,
#' it'll have a clean theme and clear fonts.
#' It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(yvar, fillby = "1", yorder = "alphanumeric", show_pct = FALSE,
#'                label_decimals = 1, label_size = 3, font_size = 14)}
#' \itemize{
#'      \item yvar     :  string, name of a categorical variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        subdividing and coloring the bars.
#'                        Default is "1", meaning no such variable is supplied.
#'      \item yorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the y-axis.
#'                        Default is "alphanumeric".
#'      \item show_pct :  logical, if TRUE, show percent on y-axis;
#'                        otherwise, show count. Default is FALSE.
#'      \item label_decimals: integer, the number of decimal points shown
#'                        on the bar labels. Default = 1.
#'      \item label_size: integer, size of bar label text. Default is 3.
#'                        Hide bar labels when its value is 0.
#'      \item font_size:  overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_barploth_freq.R
mk_barploth_freq = function(df) {
        function(yvar, fillby = "1", yorder = "alphanumeric", show_pct = FALSE,
                 label_decimals = 1, label_size = 3, font_size = 14) {

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
                        # reorder y levels in descending order of their counts
                        df[[yvar]] = reorder(df[[yvar]], df[[yvar]],
                                             function(y) length(y))
                if (yorder == "ascend")
                        # reorder y levels in ascending order of their counts
                        df[[yvar]] = reorder(df[[yvar]], df[[yvar]],
                                             function(y) -length(y))

                # get data frame of bar labels and positions
                df_label = get_bar_labels_freq(df, yvar, fillby, show_pct)

                # --- Main Plot --- #

                p = ggplot(df, aes_string(y = yvar, fill = fillby))

                if (show_pct) { # show percent instead of counts on x-axis

                        if (fillby == "1") { # single bars show pct of each y catgeory
                                p = p + ggstance::geom_barh(aes(x = ..prop.., group = 1),
                                                            alpha = 0.8) +
                                        geom_text(aes(pct, !!as.name(yvar),
                                                      label = formattable::percent(pct, label_decimals)
                                                      ),
                                                  data = df_label, hjust = -0.5,
                                                  size = label_size) +
                                        geom_text(aes(mid_pos, !!as.name(yvar),
                                                      label = scales::comma(n)),
                                                  data = df_label,
                                                  size = label_size
                                                  )
                        } else { # stacked bars of pcts by fillby var
                                p = p + ggstance::geom_barh(position = "fillv",
                                                            alpha = 0.8) +
                                        geom_text(aes(pct, !!as.name(yvar),
                                                      label = formattable::percent(pct, label_decimals)
                                                      ),
                                                  data = df_label,
                                                  size = label_size,
                                                  position = ggstance::position_stackv(hjust = 0.5)
                                                  )
                        }

                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent
                                                   )

                        xlab = "Relative Frequency (%)"

                } else { # show count on x-axis, and this is default
                        p = p + ggstance::geom_barh(position = "dodgev",
                                                    alpha = 0.8) +
                                geom_text(aes(n, !!as.name(yvar),
                                              label = scales::comma(n)),
                                          data = df_label, hjust = -0.5,
                                          size = label_size,
                                          position = ggstance::position_dodge2v(
                                                  height = 0.9)
                                          ) +
                                geom_text(aes(mid_pos, !!as.name(yvar),
                                              label = formattable::percent(pct, label_decimals)
                                              ),
                                          data = df_label, size = label_size,
                                          position = ggstance::position_dodge2v(
                                                  height = 0.9)
                                          )

                        axis_breaks = pretty(c(0, df_label$n), 10)
                        delta_size = axis_breaks[2] - axis_breaks[1]
                        p = p + scale_x_continuous(
                                expand = c(0, 0),
                                limits = c(min(axis_breaks),
                                           max(axis_breaks) + delta_size),
                                breaks = axis_breaks,
                                labels = scales::comma
                                )

                        xlab = "Frequency"
                }


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + labs(x = xlab, y = NULL) + theme_no_yaxis(font_size)

        }
}


