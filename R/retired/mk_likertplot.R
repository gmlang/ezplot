# --- This version is a direct implementation using ggplot2.
#     Con: accept cat var as xvar and con var as yvar, this is not user-friendly
#          as the output plot will have the con var on x-axis and the cat var on
#          y-axis. User has to do mental switch: x is y and y is x --- #

#' @title Create a function for making publishable ggplot2 likert plot (a.k.a.,
#' horizontal diverging bar plot), showing values of a continuous variable by a
#' 2 categorical variables.
#'
#' @description
#' \code{mk_likertplot} takes a data frame as input and returns a function for
#' making likert plots (a.k.a., horizontal diverging bar plots) with any
#' categorical variable from the data frame on the y-axis and the value of
#' any continuous variable on the x-axis, with bars
#' colored by a 2nd categorical fillby variable. The resulting plot can have
#' bars ordered from top to bottom in alphanumerical, ascending or descending
#' order.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby, fillby_lvls, xorder = "alphanumeric",
#'                y_as_pct = FALSE, font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        sub-dividing and coloring the bars.
#'      \item fillby_lvls: character vector, levels of fillby variable that the
#'                         fillby variable should be ordered accordingly.
#'      \item xorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the y-axis.
#'                        Default = "alphanumeric".
#'      \item y_as_pct :  logical, if TRUE, format y-axis and bar labels as %;
#'                        otherwise, format them as comma. Default is FALSE.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples
#' library(tidyr)
#' df = ab3 %>% gather(opinion, pct, -Country)
#' lvls = unique(df$opinion)
#' plt = mk_likertplot(df)
#' plt("Country", "pct", fillby = "opinion", fillby_lvls = lvls)
#' plt("Country", "pct", fillby = "opinion", fillby_lvls = lvls, y_as_pct = T) %>%
#'     add_labs(ylab = NULL, title = "Confidence estimates for twelve countries' economy")
#'
#' library(dplyr)
#' df = films %>% count(mpaa, made_money)
#' plt = mk_likertplot(df)
#' plt("mpaa", "n", fillby = "made_money", fillby_lvls = c("no", "yes"))
#' plt("mpaa", "n", fillby = "made_money", fillby_lvls = c("no", "yes"), xorder = "ascend")
#' plt("mpaa", "n", fillby = "made_money", fillby_lvls = c("no", "yes"), xorder = "descend")
#'
#' library(dplyr)
#' df = data.frame(item = rep(LETTERS[1:5], 4),
#'                 slope = c(rep("North", 10), rep("South", 10)),
#'                 type = rep(c(rep("native", 5), rep("introduced", 5)), 2),
#'                 spp = as.integer(abs(rnorm(20, 5, 2))))
#' df = df %>% mutate(spp = ifelse(type == "introduced", spp+1, spp),
#'                    sppInv = ifelse(type == "native", spp, spp*-1))
#'
#' # plot for only North
#' plt = mk_likertplot(df %>% filter(slope == "North"))
#'
#' # works with both negative and positive values
#' #     sppInv < 0 when type is "introduced", and > 0 when type is "native"
#' plt("item", "sppInv", fillby = "type", fillby_lvls = c("introduced", "native"))
#'
#' # also works with only positive values
#' #     spp > 0 always
#' plt("item", "spp", fillby = "type", fillby_lvls = c("introduced", "native"), xorder = "ascend")
mk_likertplot = function(df) {
        function(xvar, yvar, fillby, fillby_lvls, xorder = "alphanumeric",
                 y_as_pct = FALSE, font_size = 14) {

                # --- Prep --- #

                lst = prep_data_likert(df, xvar, yvar, fillby, fillby_lvls,
                                       xorder)
                df_neg = lst[["df_neg"]]
                df_pos = lst[["df_pos"]]
                con_axis_limits = lst[["con_axis_limits"]]
                con_axis_breaks = lst[["con_axis_breaks"]]
                con_axis_labs = lst[["con_axis_labs"]]
                pal = lst[["pal"]]


                # --- Main Plot --- #

                p = ggplot2::ggplot() +
                        ggplot2::aes_string(x = xvar, y = yvar, fill = fillby) +
                        ggplot2::geom_col(data = df_neg, alpha = 0.8) +
                        ggplot2::geom_col(data = df_pos, alpha = 0.8,
                                          position = ggplot2::position_stack(
                                                  reverse = T)) +
                        ggplot2::geom_hline(yintercept = 0, color = "white") +
                        ggplot2::scale_fill_manual(labels = fillby_lvls,
                                                   breaks = fillby_lvls,
                                                   values = pal) +
                        # make plot horizontal
                        ggplot2::coord_flip()


                # --- Format y-axis --- #

                if (y_as_pct) {
                        p = p + ggplot2::scale_y_continuous(
                                limits = c(-1, 1),
                                breaks = seq(-1, 1, 0.2),
                                labels = formattable::percent(
                                        abs(seq(-1, 1, 0.2)), 0)
                                )
                } else {
                        p = p + ggplot2::scale_y_continuous(
                                limits = con_axis_limits,
                                breaks = con_axis_breaks,
                                labels = scales::comma(con_axis_labs)
                                )
                }


                # --- Customize Theme --- #

                p + ggplot2::labs(x = NULL, y = yvar) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                        )
        }
}

