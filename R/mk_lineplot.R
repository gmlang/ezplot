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
#'                pt_size=2, ylog=F, ylog10=F, ypct=F, ypct_jump=0.2)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the lines and points.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item linew    :  numeric, the width of the lines.
#'      \item pt_size  :  numeric, the size of the points.
#'      \item ylog     :  logical, indicating whether to use log scale on y-axis. Default is FALSE.
#'      \item ylog10   :  logical, indicating whether to use log10 scale on y-axis. Default is FALSE.
#'      \item ypct     :  logical, indicating whether to use percent format on y-axis. Default is FALSE.
#'      \item ypct_jump:  numeric, between 0 and 1. Default is 0.2
#' }   
#' 
#' @export
#' @examples
#' library(ezplot)
#' 
#' # plot boxoffice/budget ratio over the years
#' plt = mk_lineplot(bo_bt_ratio_by_year)
#' title = "Boxoffice/Budget Ratio from 1913 to 2014"
#' plt("year", "bo_bt_ratio", ylab="boxoffice/budget ratio", main=title, ylog10=T)
#' plt("year", "bo_bt_ratio", ylab="boxoffice/budget ratio", ylog10=T, linew=1, pt_size=2)
#' 
#' # plot total budget and boxoffice over the years
#' plt = mk_lineplot(btbo_by_year)
#' title = "Annual Total Budget and Boxoffice from 1913 to 2014"
#' plt("year", "tot", "type", ylab="total amount ($billion)", main=title)
mk_lineplot = function(df) {
        function(xvar, yvar, fillby="", xlab="", ylab="", main="", linew=0.7, 
                 pt_size=2, ylog=F, ylog10=F, ypct=F, ypct_jump=0.2) {
                
                if (fillby == "") {
                        col = palette("blue")
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) + 
                                ggplot2::geom_line(ggplot2::aes(group = 1), color=col, size=linew) +  
                                ggplot2::geom_point(color=col, size=pt_size)
                } else {
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, group=fillby, color=fillby)) + 
                                ggplot2::geom_line(size=linew) + 
                                ggplot2::geom_point(size=pt_size)
                }
                
                p = p + ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() 
                
                if (ylog)
                        p = p + ggplot2::scale_y_continuous(trans = scales::log_trans(),
                                                            breaks = scales::trans_breaks("log", function(x) exp(x)),
                                                            labels = scales::trans_format("log", scales::math_format('e'^.x)))
                
                if (ylog10)
                        p = p + ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
                
                if (ypct)
                        p = p + ggplot2::scale_y_continuous(labels = scales::percent, 
                                                            limits = c(0, 1),
                                                            breaks = seq(0, 1, ypct_jump))                
                p
        }
}