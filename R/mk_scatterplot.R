#' @title Create a function that draws ggplot2 scatterplots.
#' 
#' @description
#' \code{mk_scatterplot} takes a data frame as input and returns a function that 
#' can be used to make scatterplots using variables in the data frame.
#' 
#' @param df A data frame.
#' @return 
#' \code{function(xvar, yvar, fillby="", xlab="", ylab="", main="", add_line=FALSE,
#'                linew=1, pt_alpha=0.5, pt_size=1)}
#' \itemize{
#'      \item xvar     :  string, x variable.
#'      \item yvar     :  string, y variable.
#'      \item fillby   :  string, variable used for coloring the points
#'      \item xlab     :  string, x-axis label.
#'      \item ylab     :  string, y-axis label.
#'      \item main     :  string, title of the plot. 
#'      \item add_line :  logical, whether to add a lm line. Default = FALSE.
#'      \item linew    :  number, default = 1.
#'      \item pt_alpha :  number, transparency of the points. Default = 0.5.
#'      \item pt_size  :  number, size of the points. Default = 1.
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
#' scale_axis(p, "y", scale = "log")
#' scale_axis(p, "x", scale = "log")
#' 
#' p = scale_axis(p, "x", scale = "log10")
#' p = scale_axis(p, "y", scale = "log10")
#' print(p)
#' 
#' p = plt("budget", "boxoffice", fillby="year_cat", xlab="budget", 
#'          ylab="boxoffice", add_line=T)
#' p = scale_axis(p, "x", scale = "log10")
#' p = scale_axis(p, "y", scale = "log10")
#' print(p)
#' 
#' p = plt("budget", "boxoffice", fillby="made_money", xlab="budget", 
#'          ylab="boxoffice", add_line=T)
#' p = scale_axis(p, "x", scale = "log2")
#' p = scale_axis(p, "y", scale = "log2")
#' print(p)
mk_scatterplot = function(df) {
        function(xvar, yvar, fillby="", xlab="", ylab="", main="",
                 add_line=F, linew=1, pt_alpha=0.5, pt_size=1) {
                
                if (fillby == "") 
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) +
                                ggplot2::geom_point(color=cb_color("blue"), 
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
