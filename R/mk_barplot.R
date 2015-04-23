#' @title Create a function that draws ggplot2 barplots
#
#' @description
#' \code{mk_barplot()} takes a data frame as input and returns a function that 
#' can be used to make barplots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' 
#' @return
#' \code{function(xvar, yvar, fillby, xlab="", ylab="", main="", 
#'                ylog=F, ylog10=F, ypct=F, ypct_jump=0.2, legend=T)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the bars.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item ylog     :  logical, indicating whether to use log scale on y-axis. Default is FALSE.
#'      \item ylog10   :  logical, indicating whether to use log10 scale on y-axis. Default is FALSE.
#'      \item ypct     :  logical, indicating whether to use percent format on y-axis. Default is FALSE.
#'      \item ypct_jump:  numeric, between 0 and 1. Default is 0.2
#'      \item legend   :  logical, indicating whether to show the legend. Default is TRUE.
#' }    
#'      
#' @export
#'     
#' @examples
#' # make some fake data
#' df = read.table(header=TRUE, text='
#' student grade
#' Joe 90
#' Mary 75
#' Alex 50')
#' 
#' # draw a barplot
#' barplt = mk_barplot(df)
#' barplt("student", "grade", "student") 
#' barplt("student", "grade", "student", legend=F) 
#' 
#' # make some fake data
#' df2 = read.table(header=TRUE, text='
#' group level val
#' A      small 1.8
#' A      medium 2.2
#' A      large 1.5
#' B      small 2.0
#' B      medium 2.6
#' B      large 1.0
#' C      small 2.5
#' C      medium 1.3
#' C      large 2.9')
#' 
#' # draw a barplot
#' barplt = mk_barplot(df2)
#' barplt("group", "val", "level") 
#' 
#' # calculate the percentage of the levels within each group
#' library(tidyr)
#' library(dplyr)
#' pct = df2 %>% spread(level, val)
#' temp = pct[, 2:4]
#' pct = cbind(group=pct[, 1], temp / apply(temp, 1, sum))
#' pct = pct %>% gather(level, pct, -group)
#' pct$level = factor(pct$level, levels=c("small", "medium", "large"))
#' 
#' # plot a stacked barplot to display the percentages
#' barplt = mk_barplot(pct)
#' barplt("group", "pct", "level", ypct=T)
mk_barplot = function(df) {
        function(xvar, yvar, fillby, xlab="", ylab="", main="", 
                 ylog=F, ylog10=F, ypct=F, ypct_jump=0.2, legend=T) {
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar, fill = fillby)) + 
                        ggplot2::geom_bar(stat = "identity") + 
                        ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw()
                
                if (ylog) 
                        p = p + ggplot2::scale_y_continuous(trans = scales::log_trans(),
                                                            breaks = scales::trans_breaks("log", function(x) exp(x)),
                                                            labels = scales::trans_format("log", scales::math_format('e'^.x)))
                else if (ylog10)
                        p = p + ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
                else if (ypct)
                        p = p + ggplot2::scale_y_continuous(labels = scales::percent, 
                                                            limits = c(0,1),
                                                            breaks = seq(0, 1, ypct_jump))
                else p = p + ggplot2::scale_y_continuous(labels = scales::comma)
                        
                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                
                p
        }
}

