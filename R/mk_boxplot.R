#' @title Create a function that draws ggplot2 boxplots
#' 
#' @description 
#' \code{mk_boxplot()} takes a data frame as input and returns a function that 
#' can be used to make boxplots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, yvar, xlab="", ylab="", main="", vpos=0, 
#'                ylog=F, ylog10=F, ypct=F, ypct_jump=0.2, legend=T)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item vpos     :  number, the additional height of the text labels beyond the max y-value of each group.
#'      \item ylog     :  logical, indicating whether to use log scale on y-axis. Default is FALSE.
#'      \item ylog10   :  logical, indicating whether to use log10 scale on y-axis. Default is FALSE.
#'      \item ypct     :  logical, indicating whether to use percent format on y-axis. Default is FALSE.
#'      \item ypct_jump:  number, between 0 and 1. Default is 0.2
#'      \item legend   :  logical, indicating whether to show legend or not. Default is TRUE.
#' }
#' @export
#' @examples 
#' library(ezplot)
#' plt = mk_boxplot(films)
#' 
#' # plot distributions of budget over the years
#' title1 = "Annual Distribution of Budget from 1913 to 2014"
#' p = plt("year", "budget", ylab="budget", main=title1, ylog10=T)
#' print(p)
#' 
#' # plot distributions of boxoffice over the years
#' title2 = "Annual Distribution of Boxoffice from 1913 to 2014"
#' p = plt("year", "boxoffice", ylab="boxoffice", main=title2, ylog10=T)
#' start = min(films$year)
#' end = max(films$year)
#' p = p + ggplot2::scale_x_continuous(limits = c(start, end), breaks = seq(start, end, 10))
#' print(p)
#' 
#' # plot distributions of boxoffice at each aggregated year level
#' p = plt("year_cat", "boxoffice", ylab="boxoffice", ylog10=T)
#' print(p)
#' 
#' # plot distributions of budget at each aggregated year level
#' p = plt("year_cat", "budget", ylab="boxoffice", ylog10=T, legend=F)
#' print(p)
mk_boxplot = function(df) {
        function(xvar, yvar, xlab="", ylab="", main="", vpos=0, 
                 ylog=F, ylog10=F, ypct=F, ypct_jump=0.2, legend=T) {
                
                xvar_type = class(df[[xvar]])
                if (xvar_type %in% c("character", "factor"))
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, fill=xvar)) +
                                ggplot2::geom_boxplot() +
                                ggplot2::stat_summary(fun.y=mean, geom="point", 
                                                      shape=5, size=2) +
                                ggplot2::stat_summary(fun.data = function(x) 
                                        c(y = max(x) + vpos, label = length(x)), 
                                        geom = "text", size = 5) 
                else
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, group=xvar)) +
                                ggplot2::geom_boxplot(color = palette("blue"))
                
                p = p + ggplot2::theme_bw() + 
                        ggplot2::labs(x = xlab, y = ylab, title = main)
                
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
                
                if (!legend)
                        p = p + ggplot2::guides(fill = FALSE)
                p
        }
}