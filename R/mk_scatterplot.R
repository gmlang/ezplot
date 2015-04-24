#' @title Create a function that draws ggplot2 scatterplots.
#' 
#' @description
#' \code{mk_scatterplot()} takes a data frame as input and returns a function that 
#' can be used to make scatterplots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, yvar, fillby="", xlab="", ylab="", main="",
#'                xlog=F, ylog=F, xlog10=F, ylog10=F, 
#'                xpct=F, ypct=F, xpct_jump=0.2, ypct_jump=0.2,
#'                pt_alpha=NULL, pt_size=NULL)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the points
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item xlog     :  logical, indicating whether to use log scale on x-axis. Default is FALSE.
#'      \item ylog     :  logical, indicating whether to use log scale on y-axis. Default is FALSE.
#'      \item xlog10   :  logical, indicating whether to use log10 scale on x-axis. Default is FALSE.
#'      \item ylog10   :  logical, indicating whether to use log10 scale on y-axis. Default is FALSE.
#'      \item xpct     :  logical, indicating whether to use percent format on x-axis. Default is FALSE.
#'      \item ypct     :  logical, indicating whether to use percent format on y-axis. Default is FALSE.
#'      \item xpct_jump:  numeric, between 0 and 1. Default is 0.2
#'      \item ypct_jump:  numeric, between 0 and 1. Default is 0.2
#'      \item add_line :  logical, indicating whether to add a lm line. Default is F.
#'      \item linew    :  numeric, default is 1.
#'      \item pt_alpha :  numeric, specifying the transparency of the points. Default is NULL.
#'      \item pt_size  :  numeric, specifying the size of the points. Default is NULL.
#' }
#' 
#' @export
#' @examples
#' library(ezplot)
#' plt = mk_scatterplot(films)
#' 
#' plt("boxoffice", "budget", xlog10=T, ylog10=T)
#' plt("boxoffice", "budget", xlog10=T, ylog10=T, add_line=T)
#' plt("boxoffice", "budget", fillby="year_cat", xlog10=T, ylog10=T)
#' plt("boxoffice", "budget", fillby="year_cat", xlog10=T, ylog10=T, add_line=T)
#' plt("boxoffice", "budget", fillby="year_cat", xlog10=T, ylog10=T, add_line=T)
#' plt("boxoffice", "budget", fillby="made_money", xlog10=T, ylog10=T, add_line=T, linew=0.5)
mk_scatterplot = function(df) {
        function(xvar, yvar, fillby="", xlab="", ylab="", main="",
                 xlog=F, ylog=F, xlog10=F, ylog10=F,  
                 xpct=F, ypct=F, xpct_jump=0.2, ypct_jump=0.2,
                 add_line=F, linew=1, pt_alpha=NULL, pt_size=NULL) {
                
                if (fillby == "") 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) +
                                ggplot2::geom_point(color=palette("blue"), 
                                                    alpha=pt_alpha, size=pt_size)
                else 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, color=fillby)) +
                                ggplot2::geom_point(alpha=pt_alpha, size=pt_size) 
                
                if (add_line)
                        p = p + ggplot2::geom_smooth(method=lm, se=F, size=linew) 
                
                p = p + ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() 
                                                
                if (xlog)
                        p = p + ggplot2::scale_x_continuous(trans = scales::log_trans(),
                                                            breaks = scales::trans_breaks("log", function(x) exp(x)),
                                                            labels = scales::trans_format("log", scales::math_format('e'^.x)))                
                else if (xlog10)
                        p = p + ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
                
                else if (xpct)
                        p = p + ggplot2::scale_x_continuous(labels = scales::percent, 
                                                            limits = c(0, 1),
                                                            breaks = seq(0, 1, xpct_jump))                
                else p = p + ggplot2::scale_x_continuous(labels = scales::comma)
                
                
                if (ylog)
                        p = p + ggplot2::scale_y_continuous(trans = scales::log_trans(),
                                                            breaks = scales::trans_breaks("log", function(x) exp(x)),
                                                            labels = scales::trans_format("log", scales::math_format('e'^.x)))
                else if (ylog10)
                        p = p + ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
                else if (ypct)
                        p = p + ggplot2::scale_y_continuous(labels = scales::percent, 
                                                            limits = c(0, 1),
                                                            breaks = seq(0, 1, ypct_jump))                
                else p = p + ggplot2::scale_y_continuous(labels = scales::comma)
                
                p
        }
}
