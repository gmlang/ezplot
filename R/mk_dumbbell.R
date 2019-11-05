#' @title Create a function for making publishable ggplot2 horizontal dumbbell
#' charts, showing range of values of a continuous variable by a
#' categorical variable.
#'
#' @description
#' \code{mk_dumbbell} takes a data frame as input and returns a function
#' for making dumbbell charts with any categorical variable from the data frame
#' on the y-axis and two continuous variables (one for left end point, and the
#' other for right end point) on the x-axis.  The resulting chart, by default,
#' will have the y categories ordered by their alphanumerical order. User can
#' choose to order the y categories in ascending/descending order of the
#' difference between the right and left end points of the x values. The chart
#' produced will also have labels for the end points, and a vertical label to
#' the far right showing the difference between right and left end points.
#' If the x values are between 0 and 1, user can choose to format the x-axis and
#' the labels as percent (%). Finally, the resulting plot has a clean theme
#' and clear fonts. It also uses color-blind friendly palettes.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar_left, xvar_right, yvar, yorder = "alphanumeric",
#'                show_pct = FALSE,
#'                color_left = "#1170aa", color_right = "#fc7d0b",
#'                size_left = 3, size_right = 3,
#'                segline_color = "#e3e2e1", segline_size = 3,
#'                dot_guide = TRUE, dot_guide_size = 0.5,
#'                label_decimals = 1, label_size = 3, font_size = 14)}
#' \itemize{
#'      \item xvar_left. String, name of a continuous x variable (left pt)
#'      \item xvar_right. String, name of a continuous x variable (right pt)
#'      \item yvar. String, name of a categorical variable for y-axis.
#'      \item yorder. String, "alphanumeric", "ascend" or "descend". It specifies
#'      how categories are ordered on the y-axis. Default = "alphanumeric".
#'      \item show_pct. Logical, if TRUE, format x-axis and point labels as %;
#'      otherwise, format them as comma. Default is FALSE.
#'      \item color_left. String, color of left point. Default is a colorblind
#'      friendly blue.
#'      \item color_right. String, color of right point. Default is a colorblind
#'      friendly orange.
#'      \item size_left. String, size of left point. Default is 3.
#'      \item size_right. String, size of right point. Default is 3.
#'      \item segline_color. String, color of the line segment connecting left
#'      and right points. Default is a gray color.
#'      \item segline_size. String, size of the line segment connecting left
#'      and right points. Default is 3.
#'      \item dot_guide. Logical, if TRUE, draw dotted guide line from
#'      0 to left point.
#'      \item dot_guide_size. Number, size of the dotted guide line.
#'      Default is 0.5.
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
#' @examples inst/examples/ex-mk_dumbbell.R
mk_dumbbell = function(df) {
        function(xvar_left, xvar_right, yvar, yorder = "alphanumeric",
                 show_pct = FALSE, color_left = "#1170aa", color_right = "#fc7d0b",
                 size_left = 3, size_right = 3,
                 segline_color = "#e3e2e1", segline_size = 3,
                 dot_guide = T, dot_guide_size = 0.5,
                 label_decimals = 1, label_size = 3, font_size = 14) {

                # --- Prep Data --- #

                dat = prep_data_dumbbell(df, xvar_left, xvar_right, yvar, yorder)



                # --- Main Plot --- #

                p = ggplot(dat) +
                        geom_segment(
                                aes_string(y = yvar, yend = yvar,
                                           x = xvar_left, xend = xvar_right),
                                color = segline_color, size = segline_size) +
                        geom_point(aes_string(xvar_left, yvar),
                                   color = color_left, size = size_left) +
                        geom_point(aes_string(xvar_right, yvar),
                                   color = color_right, size = size_right)

                if (dot_guide)
                        p = p + geom_segment(
                                        aes_string(y = yvar, yend = yvar,
                                                   x = 0, xend = xvar_left),
                                        linetype = 3, size = dot_guide_size)


                if (show_pct) {
                        # format x-axis
                        p = p + scale_x_continuous(expand = c(0, 0),
                                                   limits = c(0, 1.2),
                                                   breaks = seq(0, 1, 0.1),
                                                   labels = scales::percent
                                                   )

                        # format label text
                        label_left = formattable::percent(dat[[xvar_left]],
                                                          label_decimals)
                        label_right = formattable::percent(dat[[xvar_right]],
                                                           label_decimals)
                        label_diff = formattable::percent(dat[["diff"]],
                                                          label_decimals)

                } else {
                        # format x-axis
                        axis_breaks = pretty(
                                c(0, dat[[xvar_left]], dat[[xvar_right]]), 10)
                        delta = axis_breaks[2] - axis_breaks[1]
                        all_bigger_than1 = all(setdiff(abs(axis_breaks), 0) > 1)
                        if (all_bigger_than1)
                                con_axis_labs = scales::comma(axis_breaks)
                        else con_axis_labs = axis_breaks
                        p = p + scale_x_continuous(
                                expand = c(0, 0),
                                limits = c(min(axis_breaks),
                                           max(axis_breaks) + 2 * delta),
                                breaks = axis_breaks,
                                labels = con_axis_labs
                                )

                        # format label text
                        label_left = scales::comma(
                                dat[[xvar_left]],
                                accuracy = 1/10^label_decimals
                                )

                        label_right = scales::comma(
                                dat[[xvar_right]],
                                accuracy = 1/10^label_decimals
                                )

                        label_diff = scales::comma(
                                dat[["diff"]],
                                accuracy = 1/10^label_decimals
                                )
                }

                # --- Format labels --- #

                # add labels to end points
                p = p + geom_text(aes_string(xvar_left, yvar),
                                  label = label_left,
                                  size = label_size, vjust = 2) +
                        geom_text(aes_string(xvar_right, yvar),
                                  label = label_right,
                                  size = label_size, vjust = 2)

                # add labels of the diff column
                diff_lab_xmin = 1.1 * max(dat[[xvar_right]])
                diff_lab_xmax = 1.12 * diff_lab_xmin
                diff_lab_xmid = (diff_lab_xmin + diff_lab_xmax) / 2
                p = p + geom_rect(aes(xmin = diff_lab_xmin,
                                      xmax = diff_lab_xmax,
                                      ymin = -Inf, ymax = Inf),
                                  fill = "#efefe3") +
                        geom_text(aes_string(diff_lab_xmid, yvar),
                                  label = label_diff,
                                  fontface="bold", size = label_size)

                # add a header label above the top level on chart
                top_ylvl = levels(dat[[yvar]])[nrow(dat)]
                df_top = filter(dat, !!as.name(yvar) == top_ylvl)
                p = p + geom_text(aes_string(xvar_left, yvar),
                                  data = df_top,
                                  label = xvar_left, color = color_left,
                                  size = label_size, vjust = -2,
                                  fontface = "bold") +
                        geom_text(aes_string(xvar_right, yvar),
                                  data = df_top,
                                  label = xvar_right, color = color_right,
                                  size = label_size, vjust = -2,
                                  fontface = "bold") +
                        geom_text(aes_string(diff_lab_xmid, yvar),
                                  data = df_top,
                                  label = "DIFF", # color="#7a7d7e",
                                  size = label_size, vjust=-2,
                                  fontface="bold") +
                        scale_y_discrete(expand = c(0.075, 0))


                # # --- Format legend --- #
                #
                # # remove legend
                # p = p + guides(color = FALSE, fill = FALSE)



                # --- Customize Theme --- #

                p + labs(x = NULL, y = NULL) + theme_cowplot(font_size) +
                        theme(axis.ticks.y = element_blank())

        }
}


