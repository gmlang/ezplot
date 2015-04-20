#' @title Create a function that draws ggplot2 histograms or density plots.
#' 
#' @description
#' \code{mk_distplot()} takes a data frame as input and returns a function that 
#' can be used to make histograms or density plots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' @return 
#' \code{function(xvar, fillby="", type="histogram", binw=NULL)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item fillby    :  string, the grouping variable. Default is "".
#'      \item type     :  string, "density" or "histogram". Default is "histogram".
#'      \item binw     :  number, bin width. Default is NULL.
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
        function(xvar, fillby="", type="histogram", binw=NULL, main="") {
                
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar)) + 
                        ggplot2::labs(title = main) +
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
                
                p
        }
}

