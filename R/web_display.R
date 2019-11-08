#' @title Enlarges a ggplot2 plot for web display.
#'
#' @description
#' \code{web_display} takes a ggplot object as input, and enlarges its axis
#' tick text, axis label text, legend title and text, and distance between the
#' axis labels and the axis to make it look good for web display.
#'
#' @param p A ggplot object.
#' @param axis_text_size A number, size of the tick text. Default = 16.
#' @param axis_title_size A number, size of the axis labels. Default = rel(2).
#' @param axis_title_vjust_y A number, distance between y label and y-axis.
#' Default = 1.
#' @param mar_top A number, top margin of the whole plot. Default = 1.
#' @param mar_right A number, right margin of the whole plot. Default = 1.
#' @param mar_left A number, left margin of the whole plot. Default = 1.
#' @param mar_bottom A number, bottom margin of the whole plot. Default = 1.
#'
#' @return A ggplot object.
#'
#' @export
#' @examples inst/examples/ex-web_display.R
web_display = function(p, axis_text_size = 16, axis_title_size = rel(2),
                       axis_title_vjust_y = 1, mar_top = 1, mar_right = 1,
                       mar_left = 1, mar_bottom = 1) {
        p = p + theme(
                axis.text.x = element_text(size=axis_text_size),
                axis.text.y = element_text(size=axis_text_size),
                axis.title.x = element_text(size=axis_title_size,
                                            vjust=-axis_title_vjust_y),
                axis.title.y = element_text(size=axis_title_size,
                                            vjust=axis_title_vjust_y),

                plot.title = element_text(size=axis_title_size),

                legend.text = element_text(size=axis_text_size),
                legend.title = element_text(size=axis_title_size),

                plot.margin = grid::unit(
                        c(mar_top, mar_right, mar_left, mar_bottom), "lines")
                )

        p
}

