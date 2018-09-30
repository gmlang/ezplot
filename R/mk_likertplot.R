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
#' \code{function(xvar, yvar, fillby, fillby_lvls, yorder = "alphanumeric",
#'                x_as_pct = FALSE, font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a continuous variable for x-axis.
#'      \item yvar     :  string, name of a categorical variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        sub-dividing and coloring the bars.
#'      \item fillby_lvls: character vector, levels of fillby variable that the
#'                         fillby variable should be ordered accordingly.
#'      \item yorder   :  string, "alphanumeric", "ascend" or "descend". It
#'                        specifies how categories are ordered on the y-axis.
#'                        Default = "alphanumeric".
#'      \item x_as_pct :  logical, if TRUE, format x-axis as %;
#'                        otherwise, format it as comma. Default is FALSE.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the x-axis.
#' @export
#' @examples inst/examples/ex-mk_likertplot.R
mk_likertplot = function(df) {
        function(xvar, yvar, fillby, fillby_lvls, yorder = "alphanumeric",
                 x_as_pct = FALSE, font_size = 14) {

                # --- Prep --- #

                lst = prep_data_likert(df, xvar, yvar, fillby, fillby_lvls,
                                       yorder)
                df_neg = lst[["df_neg"]]
                df_pos = lst[["df_pos"]]
                con_axis_limits = lst[["con_axis_limits"]]
                con_axis_breaks = lst[["con_axis_breaks"]]
                con_axis_labs = lst[["con_axis_labs"]]
                pal = lst[["pal"]]

                # --- Main Plot --- #

                p = ggplot() + aes_string(x = xvar, y = yvar, fill = fillby) +
                        ggstance::geom_colh(data = df_neg, alpha = 0.8) +
                        ggstance::geom_colh(
                                data = df_pos, alpha = 0.8,
                                position = ggstance::position_stackv(reverse =T)
                                ) +
                        geom_vline(xintercept = 0, color = "white") +
                        scale_fill_manual(labels = fillby_lvls,
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
                } else {
                        all_bigger_than1 = all(setdiff(abs(con_axis_labs),0) >1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(con_axis_labs)
                        p = p + scale_x_continuous(
                                limits = con_axis_limits,
                                breaks = con_axis_breaks,
                                labels = con_axis_labs
                                )
                }


                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_cowplot(font_size)

        }
}

