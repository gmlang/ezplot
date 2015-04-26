#' @title Create a function that draws ggplot2 lineplots connectiong points
#' 
#' @description
#' \code{mk_lineplot()} takes a data frame as input and returns a function that 
#' can be used to make lineplots or multi-lineplots on variables in the data frame.
#' The variable on the x-axis often measures time, and can be categorical.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, yvar, fillby="", xlab="", ylab="", main="", linew=0.7, 
#'                pt_size=2)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the lines and points.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item linew    :  numeric, the width of the lines.
#'      \item pt_size  :  numeric, the size of the points.
#' }   
#' 
#' @export
#' @examples
#' library(ezplot)
#' 
#' # plot boxoffice/budget ratio over the years
#' plt = mk_lineplot(bo_bt_ratio_by_year)
#' title = "Boxoffice/Budget Ratio from 1913 to 2014"
#' p = plt("year", "bo_bt_ratio", ylab="boxoffice/budget ratio", main=title)
#' scale_axis(p, use_log10=T)
#' 
#' p = plt("year", "bo_bt_ratio", ylab="boxoffice/budget ratio", linew=1, pt_size=2)
#' scale_axis(p, use_log10=T)
#' 
#' # plot total budget and boxoffice over the years
#' plt = mk_lineplot(btbo_by_year)
#' title = "Annual Total Budget and Boxoffice from 1913 to 2014"
#' plt("year", "tot", "type", ylab="total amount ($billion)", main=title)
mk_lineplot = function(df) {
        function(xvar, yvar, fillby="", xlab="", ylab="", main="", linew=0.7, 
                 pt_size=2) {

                if (fillby == "") {
                        col = palette("blue")
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) + 
                                ggplot2::geom_line(ggplot2::aes(group = 1), 
                                                   color=col, size=linew) +  
                                ggplot2::geom_point(color=col, size=pt_size)
                } else {
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, 
                                                                    group=fillby, 
                                                                    color=fillby)) + 
                                ggplot2::geom_line(size=linew) + 
                                ggplot2::geom_point(size=pt_size) 
                }
                
                p = p + ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() +
                        ggplot2::theme(legend.title = ggplot2::element_blank())
                p
        }
}