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
#'                show_pct = FALSE, label_decimals = 2, label_size = 3,
#'                font_size = 14)}
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
#'                        on the bar labels. Default = 2.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'                        Hide bar labels when its value is 0.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples
#' library(ezplot)
#'
#' g = mk_barploth_resp(films)
#' p = g("boxoffice", "mpaa", yorder = "descend", font_size = 10)
#' add_labs(p, title = "Fuel efficiency generally decreases with engine size",
#'          subtitle = "Two seaters (sports cars) are an exception ...",
#'          caption = "Data from fueleconomy.gov")
#' # use label_size = 0 to remove labels
#' g("bo_bt_ratio", "mpaa", fillby = "year_cat", label_decimals = 1, label_size = 0)
#'
#' library(dplyr)
#' df = films %>% count(mpaa, made_money) %>% mutate(pct = n / sum(n))
#' g = mk_barploth_resp(df)
#' g("pct", "mpaa")
#' g("pct", "mpaa", label_decimals = 1, yorder = "descend")
#' g("pct", "mpaa", show_pct = T, label_decimals = 3, font_size = 9) %>%
#'    add_labs(ylab = NULL,
#'             title = "There're more R rated films than NC-17, PG and PG-13 combined.",
#'             subtitle = "Although a substantial portion of the films have mpaa rating info missing.",
#'             caption = "data were scrapped from imdb.com in 2010")
#' g("pct", "mpaa", fillby = "made_money", show_pct = T)
mk_barploth_resp = function(df) {
        function(xvar, yvar, fillby = "1", yorder = "alphanumeric",
                 show_pct = FALSE, label_decimals = 2, label_size = 3,
                 font_size = 14) {

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
                df_label = get_bar_labels_resp(df, gp = yvar, resp = xvar,
                                               fillby = fillby)


                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(
                                        y = yvar, weight = xvar, fill = fillby)
                                    ) +
                        ggstance::geom_barh(position = "dodgev", alpha = 0.8)

                if (show_pct) {
                        p = p + ggplot2::scale_x_continuous(
                                        limits = c(0, 1),
                                        breaks = seq(0, 1, 0.1),
                                        labels = scales::percent
                                        )
                } else {
                        p = p + ggplot2::scale_x_continuous(
                                        limits = c(min(c(0, df_label[[xvar]])),
                                                   max(df_label[[xvar]])),
                                        breaks = pretty(c(0, df_label[[xvar]]),
                                                        10),
                                        labels = scales::comma
                                        )
                }

                # --- Format bar label --- #

                if (show_pct) {
                        p = p + ggplot2::geom_text(
                                        ggplot2::aes(mid_pos,
                                                     !!as.name(yvar),
                                                     label = formattable::percent(
                                                             !!as.name(xvar),
                                                             label_decimals)
                                                     ),
                                        data = df_label,
                                        size = label_size,
                                        position = ggstance::position_dodge2v(
                                                height = 0.9)
                                        )
                } else {
                        p = p + ggplot2::geom_text(
                                        ggplot2::aes(mid_pos,
                                                     !!as.name(yvar),
                                                     label = scales::comma(
                                                             round(!!as.name(xvar),
                                                                   label_decimals))
                                                     ),
                                        data = df_label,
                                        size = label_size,
                                        position = ggstance::position_dodge2v(
                                                height = 0.9)
                                        )

                }


                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + ggplot2::guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + ggplot2::labs(x = xvar, y = NULL) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                        )

        }
}


