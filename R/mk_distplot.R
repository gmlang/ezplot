#' @title Helper function used by \code{mk_distplot}. Not for external use.
#' 
#' @description \code{draw} takes "histogram" or "density" as input and returns 
#' the corresponding ggplot2 expression to be evaluated by \code{mk_distplot}.
#' 
#' @param type string of value "histogram" or "density".
#' 
#' @return a R expression to be evaluated by \code{mk_distplot}.  
#' 
#' @seealso \code{\link{mk_distplot}}.
draw = function(type) {
        # code string for histogram
        hist_code = 'if (fillby == "") {
                        p = p + ggplot2::geom_histogram(fill = cb_color("sky_blue"), alpha=.5, binwidth=binw, position="identity")
                     } else { 
                        p = p + ggplot2::geom_histogram(ggplot2::aes_string(fill=fillby), alpha=.5, binwidth=binw, position="identity")
                     }'
        
        # code string for density
        density_code = 'if (fillby == "") {
                                p = p + ggplot2::geom_density(color = cb_color("blue"), alpha=.3, size=1)
                        } else {
                                p = p + ggplot2::geom_density(ggplot2::aes_string(color=fillby), alpha=.3)
                        }'
        
        switch(type,
               histogram = parse(text = hist_code),
               density = parse(text = density_code))
}

#' @title Create a function that draws ggplot2 histograms or density plots.
#' 
#' @description
#' \code{mk_distplot} takes a data frame as input and returns a function that can 
#' be used to make histograms or density plots using variables in the data frame.
#' It uses \code{draw} as a helper. 
#' 
#' @seealso \code{\link{draw}}.
#' 
#' @param df A data frame.
#' @return 
#' \code{function(xvar, fillby="", xlab="", type="histogram", binw=NULL, main="",
#'                add_vline_mean=FALSE, add_vline_median=FALSE)}
#' \itemize{
#'      \item xvar     :  string, x variable.
#'      \item fillby   :  string, grouping variable. Default = "".
#'      \item xlab     :  stirng, x-axis label.
#'      \item type     :  string, "density" or "histogram". Default is "histogram".
#'      \item binw     :  number, bin width. Default = NULL.
#'      \item main     :  string, title of the plot.
#'      \item add_vline_mean: logical, whether to add a vertical line at the mean. Default=FALSE.
#'      \item add_vline_median: logical, whether to add a vertical line at the median. Default=FALSE.
#' }
#' @export
#' @examples
#' f = mk_distplot(iris)
#' f("Sepal.Length")
#' f("Sepal.Length", binw=0.3)
#' f("Sepal.Length", binw=0.3, add_vline_mean=T)
#' f("Sepal.Length", binw=0.3, add_vline_median=T)
#' f("Sepal.Length", binw=0.3, add_vline_mean=T, add_vline_median=T)
#' 
#' f("Sepal.Length", type="density")
#' f("Sepal.Length", type="density", add_vline_mean=T)
#' f("Sepal.Length", type="density", add_vline_median=T)
#' f("Sepal.Length", type="density", add_vline_median=T, add_vline_mean=T)
#' 
#' f("Sepal.Length", fillby="Species")
#' f("Sepal.Length", fillby="Species", binw=0.3)
#' f("Sepal.Length", fillby="Species", binw=0.3, add_vline_mean=T)
#' f("Sepal.Length", fillby="Species", binw=0.3, add_vline_median=T)
#' f("Sepal.Length", fillby="Species", type="density")
#' f("Sepal.Length", fillby="Species", type="density", add_vline_mean=T)
#' f("Sepal.Length", fillby="Species", type="density", add_vline_median=T)
mk_distplot = function(df) {
        function(xvar, fillby="", xlab="", type="histogram", binw=NULL, main="",
                 add_vline_mean=F, add_vline_median=F) {
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar)) + 
                        ggplot2::labs(x = xlab, title = main) +
                        ggplot2::theme_bw()
                
                # draw histogram or density plot depending on the value of type
                pexpr = draw(type)                
                p = eval(pexpr)
                
                # add vline at the mean or median
                if (fillby == "") {
                        # add vline at the mean
                        if (add_vline_mean) {
                                avg = mean(df[[xvar]], na.rm=T)
                                p = p + ggplot2::geom_vline(ggplot2::aes_string(xintercept = avg),
                                                            color = cb_color("reddish_purple"), size = 1,
                                                            linetype = "dashed")                        
                        }
                        
                        # add vline at the median
                        if (add_vline_median) {
                                med = median(df[[xvar]], na.rm=T)
                                p = p + ggplot2::geom_vline(ggplot2::aes_string(xintercept = med),
                                                            color = cb_color("bluish_green"), size = 1,
                                                            linetype = "dashed")
                        }
                } else {
                        lst = split(df[,c(xvar, fillby)], df[[fillby]])
                        
                        # add vline at the mean
                        if (add_vline_mean) {
                                avg = sapply(lst, function(elt) mean(elt[[xvar]], na.rm=T))
                                means = data.frame(level=names(avg), avg)
                                p = p + ggplot2::geom_vline(data=means, 
                                                            ggplot2::aes(xintercept = avg, color=level),
                                                            linetype = "dashed", size=1)   
                        }
                        
                        # add vline at the median
                        if (add_vline_median) {
                                med = sapply(lst, function(elt) median(elt[[xvar]], na.rm=T))
                                medians = data.frame(level=names(med), med)
                                p = p + ggplot2::geom_vline(data=medians, 
                                                            ggplot2::aes(xintercept = med, color=level),
                                                            linetype = "dashed", size=1)   
                        }
                }
                p
        }
}

