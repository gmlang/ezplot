#' @title Create a function that draws ggplot2 stacked area chart.
#
#' @description
#' \code{mk_areaplot} takes a data frame as input and returns a function that 
#' can be used to make stacked area plot using the variables in the data frame.
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar, yvar, fillby, xlab="", ylab="", main="", legend=T)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the bars.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item legend   :  logical, whether to show the legend. Default is TRUE.
#' }
#' 
#' @seealso \code{\link{scale_axis}} for adding different scales to the axes. 
#' 
#' @export
#' 
#' @examples    
#' library(ezplot)
#' library(tidyr)
#' library(dplyr)
#' 
#' dat = ads %>% gather(cat, rev, -year)
#' plt = mk_areaplot(dat)
#' plt("year", "rev", fillby = "cat")
mk_areaplot = function(df) {
        function(xvar, yvar, fillby, xlab="", ylab="", main="", legend=T) {
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar, 
                                                            fill = fillby,
                                                            order = fillby)) + 
                        ggplot2::geom_area(position = "stack") + 
                        ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() +
                        ggplot2::guides(fill=ggplot2::guide_legend(reverse=TRUE))
                
                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                p
        }
}