#' @title Create a function that draws ggplot2 interval plot.
#' 
#' @description
#' \code{mk_intervalplot} takes a data frame as input and returns a function that 
#' can be used to make interval plots using variables in the data frame. An 
#' interval plot is a chart that represent an interval as a vertical line, with
#' a point in the middle.
#' 
#' @param df A data frame.
#' @return 
#' \code{function(xvar, yvar, fillby="", ymin_var, ymax_var, xlab="", ylab="", 
#'                main="", size=1)}
#' \itemize{
#'      \item xvar     :  string, x variable.
#'      \item yvar     :  string, y variable.
#'      \item fillby   :  string, variable used for coloring the points and lines.
#'      \item ymin_var :  string, variable name for ymin.
#'      \item ymax_var :  string, variable name for ymax.
#'      \item xlab     :  string, x-axis label.
#'      \item ylab     :  string, y-axis label.
#'      \item main     :  string, title of the plot. 
#'      \item size     :  number, size of the points and lines. Default = 1.
#'      \item legend   :  logical, whether to show the legend. Default is TRUE.
#' }
#' 
#' @export
#' @examples
#' library(ezplot)
#' fit = lm(log10(boxoffice) ~ year_cat, data=films)
#' pred = predict(fit, films, interval="prediction")
#' dat = data.frame(year_cat=films$year_cat, pred)
#' plt = mk_intervalplot(dat)
#' p = plt("year_cat", "fit", ymin_var="lwr", ymax_var="upr", 
#'         ylab="log10(boxoffice) prediction")
#' p
mk_intervalplot = function(df) {
        function(xvar, yvar, fillby="", ymin_var, ymax_var,
                 xlab="", ylab="", main="", size=1, legend=T) {
                
                if (fillby == "") 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar,
                                                                    ymin=ymin_var,
                                                                    ymax=ymax_var)) +
                                ggplot2::geom_pointrange(color=cb_color("blue"), size=size)
                else 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, 
                                                                    ymin=ymin_var,
                                                                    ymax=ymax_var,
                                                                    color=fillby)) +
                                ggplot2::geom_pointrange(size=size) 
                
                p = p + ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() 

                if (!legend) p = p + ggplot2::guides(color = FALSE)
                p
        }
}
