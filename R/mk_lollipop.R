#' @title Create a function for making publishable ggplot2 horizontal lollipop
#' charts, showing single or cumulative values of a continuous variable by a
#' categorical variable.
#'
#' @description
#' \code{mk_lollipop} takes a data frame as input and returns a function
#' for making horizontal lollipop charts with any categorical variable from the
#' data frame on y-axis and the value (or cumulative values) of any continuous
#' variable on x-axis. The resulting chart will have the y levels ordered
#' alphanumerically by default. It'll label the stems of lollipops with the
#' cumulative x values. If the x values are decimals, user can specify the
#' number of decimals to display. If the x values are between 0 and 1,
#' user can choose to format the x-axis and the labels as percent (%). Finally,
#' the resulting plot will have a clean theme and clear fonts.
#' It'll also use color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, yorder = "alphanumeric", show_pct = FALSE,
#'                label_decimals = 1, label_size = 3, font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item yvar. String, name of a categorical variable for y-axis.
#'      \item yorder. String, "alphanumeric", "ascend" or "descend". It specifies
#'      how categories are ordered on the y-axis. Default = "alphanumeric".
#'      \item show_pct. Logical, if TRUE, format x-axis and bar labels as %;
#'      otherwise, format them as comma. Default is FALSE.
#'      \item label_decimals. Integer, the number of decimal points shown on
#'      the bar labels. Default = 1.
#'      \item label_size. Integer, size of bar label text. Default = 3. Hide bar
#'      labels when its value is 0.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @seealso \code{\link{scale_axis}} for adding different scales to the y-axis.
#' @export
#' @examples inst/examples/ex-mk_lollipop.R
mk_lollipop = function(df) {
        function(xvar, yvar, yorder = "alphanumeric", show_pct = FALSE,
                 label_decimals = 1, label_size = 3, font_size = 14) {

                # --- Prep --- #

                dat = prep_data_lollipop(df, xvar, yvar, yorder)

                # --- Main Plot --- #

                p = ggplot(dat, aes_string(xvar, yvar)) +
                        geom_segment(aes_string(x = 0, y = yvar, xend = xvar,
                                                yend = yvar),
                                     color = "black") +
                        geom_point(color = "steelblue", size = 3)


                if (show_pct) {
                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent
                                                   )

                        # format lollipop label text
                        pop_labels = formattable::percent(dat[[xvar]],
                                                          label_decimals)

                } else {
                        axis_breaks = pretty(c(0, dat[[xvar]]), 10)
                        delta_size = axis_breaks[2] - axis_breaks[1]
                        all_bigger_than1 = all(setdiff(abs(axis_breaks), 0) > 1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(axis_breaks)
                        else con_axis_labs = axis_breaks

                        p = p + scale_x_continuous(
                                expand = c(0, 0),
                                limits = c(min(axis_breaks),
                                           max(axis_breaks) + delta_size),
                                breaks = axis_breaks,
                                labels = con_axis_labs
                                )

                        # format lollipop label text
                        pop_labels = scales::comma(
                                dat[[xvar]],
                                accuracy = 1/10^label_decimals
                                )
                }

                # --- Format bar label --- #

                p = p + geom_text(aes(mid_pos, !!as.name(yvar)),
                                  label = pop_labels, size = label_size,
                                  vjust = -0.5)


                # --- Format Legend --- #

                # remove legend
                p = p + guides(color = FALSE, fill = FALSE)



                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_cowplot(font_size) +
                        theme(axis.ticks.y = element_blank())

        }
}


