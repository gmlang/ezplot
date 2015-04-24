#' @title Create a function that draws ggplot2 scatterplots.
#' 
#' @description
#' \code{mk_scatterplot()} takes a data frame as input and returns a function that 
#' can be used to make scatterplots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, yvar, fillby="", xlab="", ylab="", main="", add_line=F,
#'                linew=1, pt_alpha=0.5, pt_size=1)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the points
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item add_line :  logical, indicating whether to add a lm line. Default is F.
#'      \item linew    :  numeric, default is 1.
#'      \item pt_alpha :  numeric, specifying the transparency of the points. Default is 0.5.
#'      \item pt_size  :  numeric, specifying the size of the points. Default is 1.
#' }
#' 
#' @export
#' @examples
#' library(ezplot)
#' plt = mk_scatterplot(films)
#' 
#' plt("budget", "boxoffice", xlab="budget", ylab="boxoffice", 
#'      main="Boxoffice vs. Budget")
#' 
#' p = plt("budget", "boxoffice", xlab="budget", ylab="boxoffice", 
#'          pt_alpha=0.2, pt_size=1.5, add_line=T, linew=0.8)
#' scale_axis(p, "y", use_log=T)
#' scale_axis(p, "x", use_log=T)
#' 
#' p = scale_axis(p, "x", use_log10=T)
#' p = scale_axis(p, "y", use_log10=T)
#' print(p)
#' 
#' p = plt("budget", "boxoffice", fillby="year_cat", xlab="budget", 
#'          ylab="boxoffice", add_line=T)
#' p = scale_axis(p, "x", use_log10=T)
#' p = scale_axis(p, "y", use_log10=T)
#' print(p)
#' 
#' p = plt("budget", "boxoffice", fillby="made_money", xlab="budget", 
#'          ylab="boxoffice", add_line=T)
#' p = scale_axis(p, "x", use_log10=T)
#' p = scale_axis(p, "y", use_log10=T)
#' print(p)
mk_scatterplot = function(df) {
        function(xvar, yvar, fillby="", xlab="", ylab="", main="",
                 add_line=F, linew=1, pt_alpha=0.5, pt_size=1) {
                
                if (fillby == "") 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) +
                                ggplot2::geom_point(color=palette("blue"), 
                                                    alpha=pt_alpha, 
                                                    size=pt_size)
                else 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, 
                                                                    color=fillby)) +
                                ggplot2::geom_point(alpha=pt_alpha, 
                                                    size=pt_size) 
                
                if (add_line)
                        p = p + ggplot2::geom_smooth(method=lm, se=F, size=linew) 
                
                p = p + ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() 
                
                p
        }
}
