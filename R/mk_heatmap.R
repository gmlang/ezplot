#' @title Create a function that draws ggplot2 heat maps.
#
#' @description
#' \code{mk_heatmap} takes a data frame as input and returns a function that 
#' can be used to make heat maps using variables in the data frame.
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar, yvar, fillby, xlab="", ylab="", main="", base_size = 12, 
#'                use_theme_gray=TRUE, legend=TRUE)}
#' \itemize{
#'      \item xvar     :  string, x variable.
#'      \item yvar     :  string, y variable.
#'      \item fillby   :  string, variable used for coloring the bars.
#'      \item xlab     :  string, x-axis label.
#'      \item ylab     :  string, y-axis label.
#'      \item main     :  string, title of the plot. 
#'      \item base_size:  numeric, size of tick text for both axes. Default = 12.
#'      \item use_theme_gray:  logical, whether to use theme_gray or theme_minimal. Default = TRUE.
#'      \item legend   :  logical, whether to show the legend. Default = TRUE.
#' }    
#'      
#' @export
#'     
mk_heatmap = function(df) {
        function(xvar, yvar, fillby, xlab="", ylab="", main="", base_size = 12, 
                 use_theme_gray=T, legend=T) {
                p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar))
                
                if (use_theme_gray)
                        p = p + ggplot2::theme_gray(base_size=base_size) 
                else 
                        p = p + ggplot2::theme_minimal(base_size=base_size)
                
                p = p + ggplot2::geom_tile(ggplot2::aes_string(fill=fillby), 
                                           color="white") + 
                        ggplot2::scale_fill_gradient(low="white", 
                                                     high="steelblue") +
                        ggplot2::labs(x=xlab, y=ylab, title=main) +
                        ggplot2::scale_x_discrete(expand = c(0, 0)) +
                        ggplot2::scale_y_discrete(expand = c(0, 0)) +
                        ggplot2::theme(axis.ticks = ggplot2::element_blank())
                                
                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                
                p
        }
}

