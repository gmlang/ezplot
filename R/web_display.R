#' @title Enlarges a ggplot2 plot for web display
#' 
#' @description
#' \code{web_display()} takes a ggplot object as input, and enlarges its axis 
#' tick text, axis label text, legend title and text, and distance between the 
#' axis labels and the axis to make it look good for web display.
#' 
#' @param p ggplot object
#' @param axis_text_size number, the size of the tick text. Default = 16.
#' @param axis_title_size number, the size of the axis labels. Default = rel(2).
#' @param axis_title_vjust_y number, the distance between y label and y-axis. Default = 1. 
#' @param legend_pos string, the position of the legend. Default = "right". 
#' @param legend_title logical, whether to display legend title. Default = TRUE.
#' @param mar_top number, top margin of the whole plot. Default = 1.
#' @param mar_right number, right margin of the whole plot. Default = 1.
#' @param mar_left number, left margin of the whole plot. Default = 1.
#' @param mar_bottom number, bottom margin of the whole plot. Default = 1.
#' 
#' @return a ggplot object
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
#' # make the barplot web-ready
#' web_display(p)
#' web_display(p, legend_pos="top")
#' web_display(p, legend_pos="none")
#' web_display(p, legend_pos="bottom")
web_display = function(p, axis_text_size=16, axis_title_size=ggplot2::rel(2),
                       axis_title_vjust_y=1, legend_pos="right", legend_title=T,
                       mar_top=1, mar_right=1, mar_left=1, mar_bottom=1) {
        p = p + ggplot2::theme(
                axis.text.x = ggplot2::element_text(size=axis_text_size),
                axis.text.y = ggplot2::element_text(size=axis_text_size),
                axis.title.x = ggplot2::element_text(size=axis_title_size,
                                                     vjust=-axis_title_vjust_y),
                axis.title.y = ggplot2::element_text(size=axis_title_size,
                                                     vjust=axis_title_vjust_y),
                
                plot.title = ggplot2::element_text(size=axis_title_size),
                
                legend.text = ggplot2::element_text(size=axis_text_size),
                legend.title = ggplot2::element_text(size=axis_title_size),
                legend.position = legend_pos,
                
                plot.margin = grid::unit(c(mar_top, mar_right, 
                                           mar_left, mar_bottom), "lines")
        )
        
        if (!legend_title)
                p = p + ggplot2::theme(legend.title = ggplot2::element_blank())
        
        p
}

