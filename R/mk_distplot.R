#' @title Create a function that draws ggplot2 histograms or density plots.
#' 
#' @description
#' \code{mk_distplot()} takes a data frame as input and returns a function that 
#' can be used to make histograms or density plots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, fillby="", xlab="", type="histogram", binw=NULL, 
#'                main="", xlog=F, xlog10=F, xpct=F, xpct_jump=0.2)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item fillby   :  string, the grouping variable. Default is "".
#'      \item type     :  string, "density" or "histogram". Default is "histogram".
#'      \item binw     :  number, bin width. Default is NULL.
#'      \item xlab     :  string, the x-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item xlog     :  logical, indicating whether to use log scale on x-axis. Default is FALSE.
#'      \item xlog10   :  logical, indicating whether to use log10 scale on x-axis. Default is FALSE.
#'      \item xpct     :  logical, indicating whether to use percent format on x-axis. Default is FALSE.
#'      \item xpct_jump:  numeric, between 0 and 1. Default is 0.2
#' }
#' @export
#' @examples
#' f = mk_distplot(iris)
#' f("Sepal.Length")
#' f("Sepal.Length", binw=0.3)
#' f("Sepal.Length", type="density")
#' 
#' f("Sepal.Length", fillby="Species")
#' f("Sepal.Length", fillby="Species", binw=0.3)
#' f("Sepal.Length", fillby="Species", type="density")
mk_distplot = function(df) {
        function(xvar, fillby="", xlab="", type="histogram", binw=NULL, 
                 main="", xlog=F, xlog10=F, xpct=F, xpct_jump=0.2) {
                
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar)) + 
                        ggplot2::labs(x = xlab, title = main) +
                        ggplot2::theme_bw()
                
                if (type == "histogram") {
                        if (fillby == "") 
                                p = p + ggplot2::geom_histogram(fill = palette("blue"),
                                                                alpha=.5, binwidth=binw,
                                                                position="identity") 
                        
                        else 
                                p = p + ggplot2::geom_histogram(ggplot2::aes_string(fill=fillby), 
                                                                alpha=.5, binwidth=binw,
                                                                position="identity")
                }
                
                if (type == "density") {
                        if (fillby == "") 
                                p = p + ggplot2::geom_density(color = palette("blue"), alpha=.3)
                        else
                                p = p + ggplot2::geom_density(ggplot2::aes_string(color=fillby), alpha=.3)
                }
                
                if (fillby == "") {
                        # add vline at the mean
                        p = p + ggplot2::geom_vline(ggplot2::aes_string(xintercept = mean(df[[xvar]], na.rm=T)),
                                                    color = palette("red"), linetype = "dashed")                        
                }
                
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
                
                p
        }
}

