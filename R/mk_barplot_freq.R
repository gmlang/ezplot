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
#' fillby variable. The resulting bar chart will have bars ordered from tallest
#' to shortest by default, and labeled with both counts and percents. Plus,
#' it'll have a clean theme, clear fonts, and a 1:1 aspect ratio (i.e., it'll be
#' a square). It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, fillby = "1", xorder = "alphanumeric", show_pct = FALSE,
#'                label_size = 3, font_size = 14, xlab = NULL, ylab = "Count",
#'                ...)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        breaking down the counts or percents of each bar.
#'                        Default is "1", meaning no such variable is supplied.
#'      \item xorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the x-axis.
#'                        Default is "alphanumeric".
#'      \item show_pct :  logical, if TRUE, show percent on y-axis;
#'                        otherwise, show count. Default is FALSE.
#'      \item label_size: integer, size of bar label text. Default is 3.
#'      \item font_size:  overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item xlab     :  string, the x-axis label. Default is NULL.
#'      \item ylab     :  string, the y-axis label. Default is "Count".
#'      \item ...      :  other arguments for ggplot2::labs(), for example,
#'                        title, subtitle, caption and etc.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples
#' library(ezplot)
#' f = mk_barplot_freq(films)
#'
#' f("mpaa")
#' f("mpaa", xorder = "descend")
#' f("year_cat", font_size = 10)
#' f("year_cat", fillby = "made_money")
#' f("year_cat", fillby = "made_money", xorder = "descend")
#' f("mpaa", fillby = "made_money")
#' f("made_money", fillby = "year_cat")
#' f("made_money", fillby = "year_cat", xorder = "descend")
#' f("year_cat", fillby = "mpaa")
#'
#' f("mpaa", show_pct = T)
#' f("mpaa", show_pct = T, xorder = "descend")
#' f("mpaa", fillby = "made_money", show_pct = T)
#' f("mpaa", fillby = "made_money", show_pct = T, xorder = "descend")
#' f("year_cat", fillby = "made_money", show_pct = T)
#' f("year_cat", fillby = "mpaa", show_pct = T)
#'
#' f = mk_barplot_freq(ggplot2::diamonds)
#' f("cut")
#' f("cut", show_pct = T, font_size = 9)
#' f("cut", show_pct = T, fillby = "clarity") +
#'     ggplot2::theme(legend.position = "bottom") +
#'     ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
#' f("cut", fillby = "clarity", font_size = 11,
#'   title = "Fuel efficiency generally decreases with engine size",
#'   subtitle = "Two seaters (sports cars) are an exception ...",
#'   caption = "Data from fueleconomy.gov")
mk_barplot_freq = function(df) {
        function(xvar, fillby = "1", xorder = "alphanumeric", show_pct = FALSE,
                 label_size = 3, font_size = 14, xlab = NULL, ylab = "Count",
                 ...) {

                # --- Prep --- #

                if (xorder == "descend")
                        # reorder xvar levels in descending order of their counts
                        df[[xvar]] = reorder(df[[xvar]], df[[xvar]],
                                             function(x) -length(x))
                if (xorder == "ascend")
                        # reorder xvar levels in ascending order of their counts
                        df[[xvar]] = reorder(df[[xvar]], df[[xvar]],
                                             function(x) length(x))

                # get data frame of bar labels and positions
                df_label = get_bar_labels_freq(df, xvar, fillby, show_pct)

                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(xvar, fill = fillby))

                if (show_pct) { # show percent instead of counts on y-axis

                        if (fillby == "1") { # single bars show pct of each x catgeory
                                p = p + ggplot2::geom_bar(
                                                ggplot2::aes(y = ..prop.., group = 1),
                                                alpha = 0.8) +
                                        ggplot2::geom_text(
                                                ggplot2::aes(!!as.name(xvar), pct,
                                                             label = formattable::percent(pct, 1)),
                                                data = df_label, vjust = -0.5,
                                                size = label_size) +
                                        ggplot2::geom_text(
                                                ggplot2::aes(
                                                        !!as.name(xvar),
                                                        mid_pos_y,
                                                        label = scales::comma(n)),
                                                data = df_label,
                                                size = label_size)
                        } else { # stacked bars of pcts by fillby var
                                p = p + ggplot2::geom_bar(position = "fill",
                                                          alpha = 0.8) +
                                        ggplot2::geom_text(
                                                ggplot2::aes(!!as.name(xvar), pct,
                                                             label = formattable::percent(pct, 1)),
                                                data = df_label,
                                                size = label_size,
                                                position = ggplot2::position_stack(vjust = 0.5)
                                        )
                        }

                        p = p + ggplot2::scale_y_continuous(
                                        labels = scales::percent,
                                        limits = c(0, 1),
                                        breaks = seq(0, 1, 0.2))

                        ylab = NULL # because y is percent, no need to label so

                } else { # show count on y-axis, and this is default
                        p = p + ggplot2::geom_bar(position = "dodge", alpha = 0.8) +
                                ggplot2::scale_y_continuous(labels = scales::comma) +
                                ggplot2::geom_text(
                                        ggplot2::aes(!!as.name(xvar), n,
                                                     label = scales::comma(n)),
                                        data = df_label,
                                        vjust = -0.5,
                                        size = label_size,
                                        position = ggplot2::position_dodge(width = 0.9)
                                        ) +
                                ggplot2::geom_text(
                                        ggplot2::aes(!!as.name(xvar), mid_pos_y,
                                                     label = formattable::percent(pct, 1)),
                                        data = df_label, size = label_size,
                                        position = ggplot2::position_dodge(width = 0.9)
                                        )
                }


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + ggplot2::guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + ggplot2::labs(x = xlab, y = ylab, ...) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                aspect.ratio = 1,

                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                                )
        }
}


