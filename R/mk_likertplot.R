#' @title Create a function for making publishable ggplot2 likert plot (a.k.a.,
#' horizontal diverging bar plot), showing values of a continuous variable by a
#' 2 categorical variables.
#'
#' @description
#' \code{mk_likertplot} takes a data frame as input and returns a function for
#' making likert plots (a.k.a., horizontal diverging bar plots) with any
#' categorical variable from the data frame on the y-axis and the value of
#' any continuous variable on the x-axis, with bars colored by a 2nd
#' categorical fillby variable. The resulting plot can have bars ordered from
#' top to bottom in alphanumerical, ascending or descending order.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby, fillby_lvls, rawcnt_var = NULL,
#'                yorder = NULL, x_as_pct = FALSE,
#'                label_decimals = 1, label_size = 3,
#'                legend_title = fillby, legend_pos = "right",
#'                grid_line_size = 0.4, font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item yvar. String, name of a categorical variable for y-axis.
#'      \item fillby. String, name of a different categorical variable for
#'      sub-dividing and coloring the bars.
#'      \item fillby_lvls. Character vector, levels of fillby variable that the
#'      fillby variable should be ordered accordingly.
#'      \item rawcnt_var. String, name of the variable that contains the raw
#'      count behind each tier. Default = NULL because not all input dataframe
#'      has such a variable.
#'      \item yorder String. Possible values: NULL (default), "alphanumeric",
#'      "ascend" or "descend". It specifies how categories are ordered on the
#'      y-axis. When NULL, the categories are shown in their order in the data.
#'      \item x_as_pct. Logical, if TRUE, format x-axis as %; otherwise, format
#'      it as comma. Default is FALSE.
#'      \item label_decimals. Integer, the number of decimal points shown on
#'      the bar labels. Default = 1.
#'      \item label_size. Integer, size of bar label text. Default is 3. Hide
#'      bar labels when its value is 0.
#'      \item legend_title. String, legend title. Default is the name of the
#'      fillby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item grid_line_size. Number, the width of vertical grid lines.
#'      If 0 (default), the vertical grid lines are hidden.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the x-axis.
#' @export
#' @examples inst/examples/ex-mk_likertplot.R
mk_likertplot = function(df) {
        function(xvar, yvar, fillby, fillby_lvls, rawcnt_var = NULL,
                 yorder = NULL, x_as_pct = FALSE, show_rawcnt = FALSE,
                 label_decimals = 1, label_size = 3, legend_title = fillby,
                 legend_pos = "right", grid_line_size = 0, font_size = 14) {

                # --- Prep --- #

                lst = prep_data_likert(df, xvar, yvar, fillby, fillby_lvls,
                                       rawcnt_var, yorder)
                df_neg = lst[["df_neg"]]
                df_pos = lst[["df_pos"]]
                df_neg_pos = lst[['df_neg_pos']]
                con_axis_limits = lst[["con_axis_limits"]]
                con_axis_breaks = lst[["con_axis_breaks"]]
                con_axis_labs = lst[["con_axis_labs"]]
                pal = lst[["pal"]]

                # get bar labels
                bar_labs_prime = ifelse(df_neg_pos[[xvar]] == 0, NA,
                                        abs(df_neg_pos[[xvar]]))
                if (!is.null(rawcnt_var))
                        bar_labs_rawcnt = ifelse(
                                df_neg_pos[[rawcnt_var]] == 0, NA,
                                df_neg_pos[[rawcnt_var]])


                # --- Main Plot --- #

                p = ggplot() + aes_string(x = xvar, y = yvar, fill = fillby) +
                        ggstance::geom_colh(data = df_neg, alpha = 0.8) +
                        ggstance::geom_colh(
                                data = df_pos, alpha = 0.8,
                                position = ggstance::position_stackv(reverse =T)
                                ) +
                        geom_vline(xintercept = 0, color = "white") +
                        scale_fill_manual(name = legend_title,
                                          labels = fillby_lvls,
                                          breaks = fillby_lvls,
                                          values = pal)

                # --- Format x-axis --- #

                if (x_as_pct) {
                        xbreaks = seq(-1, 1, 0.2)
                        p = p + scale_x_continuous(
                                limits = c(-1, 1),
                                breaks = xbreaks,
                                labels = formattable::percent(abs(xbreaks), 0)
                                )
                        bar_labs_prime = formattable::percent(
                                bar_labs_prime, label_decimals)
                } else {
                        all_bigger_than1 = all(setdiff(abs(con_axis_labs),0) >1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(con_axis_labs)
                        p = p + scale_x_continuous(
                                limits = con_axis_limits,
                                breaks = con_axis_breaks,
                                labels = con_axis_labs
                                )
                        bar_labs_prime = round(bar_labs_prime, label_decimals)
                }

                # --- Add Bar Labels --- #

                if (show_rawcnt) {
                        bar_labs = paste(bar_labs_prime,
                                         paste0('(', bar_labs_rawcnt, ')'),
                                         sep = '\n')
                        bar_labs[grepl('NA', bar_labs)] = NA_character_
                } else {
                        bar_labs = bar_labs_prime
                }
                p = p + geom_text(data = df_neg_pos,
                                  aes_string('mid_pos', yvar, group = fillby),
                                  label = bar_labs, size = label_size)

                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_cowplot(font_size) +
                        theme(legend.position = legend_pos,
                              # vertical grid lines are hidden when grid_line_size = 0
                              panel.grid.minor = element_line(
                                      colour = "grey83", size = grid_line_size)
                              )

        }
}
