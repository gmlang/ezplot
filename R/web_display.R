#' @title Enlarges a ggplot2 plot for web display
#' 
#' @description
#' \code{web_display()} takes a ggplot object as input, and enlarges its axis 
#' tick text, axis label text, legend title and text, and distance between the 
#' axis labels and the axis to make it look good for web display.
#' 
#' @param p ggplot object
#' 
#' @return a ggplot object
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
#' barplt = mk_barplot(df)
#' p = barplt("student", "grade", "student", xlab="Student", ylab="Grade") 
#' print(p)
#' 
#' # make the barplot web-ready
#' web_display(p)
#' web_display(p, legend_pos="top")
#' web_display(p, legend_pos="none")
#' web_display(p, legend_pos="bottom")
web_display = function(p, axis_text_size=20, axis_title_size=ggplot2::rel(2),
                       axis_text_angle_x=0,
                       axis_title_vjust_y=1,
                       legend_pos="right",
                       mar_top=1, mar_right=1, mar_left=1, mar_bottom=1) {
        p = p + ggplot2::theme(
                axis.text.x = ggplot2::element_text(size=axis_text_size,
                                                    angle=axis_text_angle_x),
                axis.text.y = ggplot2::element_text(size=axis_text_size),
                legend.text = ggplot2::element_text(size=axis_text_size),
                axis.title.x = ggplot2::element_text(size=axis_title_size,
                                                     vjust=-axis_title_vjust_y),
                axis.title.y = ggplot2::element_text(size=axis_title_size,
                                                     vjust=axis_title_vjust_y),
                legend.title = ggplot2::element_text(size=axis_title_size),
                legend.position = legend_pos,
                plot.margin = grid::unit(c(mar_top, mar_right, 
                                           mar_left, mar_bottom), "lines")
        )
        p
}

