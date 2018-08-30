#' @title Create a function for making publishable ggplot2 barplots, showing
#' single or cumulative values of a continuous variable by a categorical
#' variable or 2 categorical variables.
#'
#' @description
#' \code{mk_barplot_yvar} takes a data frame as input and returns a function for
#' making bar charts with any categorical variable from the data frame on the
#' x-axis and the value (or cumulative values) of any continuous variable on
#' the y-axis. The output function can also produce dodged bar charts when
#' supplied a second categorical variable, a fillby variable. The resulting
#' bar chart will have bars ordered from tallest to shortest, and labeled with
#' the y values. If the y values are decimals, user can specify the number of
#' decimals to display on the bar labels. If the y values are between 0 and 1,
#' user can choose to format the y-axis and the bar labels as percent (%). Plus,
#' the resulting plot will have a clean theme, clear fonts, and a 1:1 aspect
#' ratio (i.e., it's a square). It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby = "1", xorder = "alphanumeric",
#'                y_as_pct = FALSE, label_decimals = 2, label_size = 3,
#'                font_size = 14, xlab = NULL, ylab = yvar, ...)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        breaking down the y values of each bar. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item xorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the x-axis.
#'                        Default = "alphanumeric".
#'      \item y_as_pct :  logical, if TRUE, format y-axis and bar labels as %;
#'                        otherwise, format them as comma. Default is FALSE.
#'      \item label_decimals: integer, the number of decimal points shown
#'                        on the bar labels. Default = 2.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item xlab     :  string, the x-axis label. Default is NULL.
#'      \item ylab     :  string, the y-axis label. Default is yvar.
#'      \item ...      :  other arguments for ggplot2::labs(), for example,
#'                        title, subtitle, caption and etc.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples
#' library(ezplot)
#' g = mk_barplot_yvar(films)
#'
#' g("mpaa", "boxoffice", xorder = "descend", font_size = 10,
#'   title = "Fuel efficiency generally decreases with engine size",
#'   subtitle = "Two seaters (sports cars) are an exception because of ...",
#'   caption = "Data from fueleconomy.gov")
#' g("mpaa", "bo_bt_ratio", fillby = "year_cat", label_decimals = 1)
#'
#' library(dplyr)
#' df = films %>% count(mpaa) %>% mutate(pct = n / sum(n))
#' g = mk_barplot_yvar(df)
#' g("mpaa", "pct")
#' g("mpaa", "pct", label_decimals = 1)
#' g("mpaa", "pct", y_as_pct = T, font_size = 9)
#' g("mpaa", "pct", y_as_pct = T, label_decimals = 3)
#' g("mpaa", "pct", y_as_pct = T, xorder = "descend")
#' g("mpaa", "n", ylab = "Count")
mk_barplot_yvar = function(df) {
        function(xvar, yvar, fillby = "1", xorder = "alphanumeric",
                 y_as_pct = FALSE, label_decimals = 2, label_size = 3,
                 font_size = 14, xlab = NULL, ylab = yvar, ...) {

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
                df_label = get_bar_labels_yvar(df, xvar, yvar, fillby)

                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(xvar, fill = fillby)) +
                        ggplot2::geom_bar(ggplot2::aes_string(weight = yvar),
                                          position = "dodge", alpha = 0.8)

                if (y_as_pct) {
                        p = p + ggplot2::scale_y_continuous(
                                        labels = scales::percent,
                                        limits = c(0, 1),
                                        breaks = seq(0, 1, 0.2)) +
                                ggplot2::geom_text(
                                        ggplot2::aes(!!as.name(xvar), !!as.name(yvar),
                                                     label = formattable::percent(!!as.name(yvar),
                                                                                  label_decimals)),
                                        data = df_label,
                                        vjust = -0.5,
                                        size = label_size)
                        ylab = NULL
                } else {
                        p = p + ggplot2::scale_y_continuous(labels = scales::comma) +
                                ggplot2::geom_text(
                                        ggplot2::aes(!!as.name(xvar), !!as.name(yvar),
                                                     label = scales::comma(
                                                             round(!!as.name(yvar), label_decimals))
                                                     ),
                                        data = df_label,
                                        vjust = -0.5,
                                        size = label_size,
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


