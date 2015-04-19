#' @title Rotate axis tick text
#' 
#' @param p ggplot2 object.
#' @param axis_text_angle_x the angle of the tick text on x-axis. Default=45.
#' @param axis_text_angle_y the angle of the tick text on y-axis. Default=0.     
#' @return a ggplot2 object.
#' @export
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
#' # rotate the x-axis tick text 
#' rotate_axis_text(p)
#' rotate_axis_text(p, 90)
#' rotate_axis_text(p, 90, 30)
rotate_axis_text = function(p, axis_text_angle_x=45, axis_text_angle_y=0) {
        p = p + ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle=axis_text_angle_x), 
                axis.text.y = ggplot2::element_text(angle=axis_text_angle_y)
                )        
        p
}