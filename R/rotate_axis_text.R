#' @title Rotate axis tick text and adjust their distances to the axis.
#'
#' @param p A ggplot2 object.
#' @param text_angle_x Angle of tick text on x-axis. Default = 45.
#' @param text_angle_y Angle of tick text on y-axis. Default = 0.
#' @param hjust_x Vertical distance adjustment of tick text to x-axis. Default = NULL.
#' @param hjust_y Horizontal distance adjustment of tick text to y-axis. Default = NULL.
#' @param vjust_x Horizontal distance adjustment of tick text to x-axis. Default = NULL.
#' @param vjust_y Vertical distance adjustment of tick text to y-axis. Default = NULL.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' # make some data
#' df = read.table(header=TRUE, text='
#' student grade
#' Joe 90
#' Mary 75
#' Alex 50')
#'
#' # draw a barplot
#' plt = mk_barplot_yvar(df)
#' p = plt("student", "grade")
#' print(p)
#'
#' # rotate the x-axis tick text
#' rotate_axis_text(p)
#' rotate_axis_text(p, 90)
#' rotate_axis_text(p, 90, vjust_x = 0.4)
rotate_axis_text = function(p, text_angle_x = 45, text_angle_y = 0,
                            hjust_x = NULL, hjust_y = NULL,
                            vjust_x = NULL, vjust_y = NULL) {
        p + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                        angle = text_angle_x, hjust = hjust_x, vjust = vjust_x),
                axis.text.y = ggplot2::element_text(
                        angle = text_angle_y, hjust = hjust_y, vjust = vjust_y)
                )
}
