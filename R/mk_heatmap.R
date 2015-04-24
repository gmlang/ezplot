#' @title Create a function that draws ggplot2 heatmaps
#
#' @description
#' \code{mk_heatmap()} takes a data frame as input and returns a function that 
#' can be used to make heatmaps on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' 
#' @return
#' \code{function(xvar, yvar, fillby, xlab="", ylab="", main="", base_size = 12, 
#'                use_theme_gray=T, legend=T)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the bars.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item base_size:  numeric, the size of both axis tick text, default = 12.
#'      \item use_theme_gray:  logical, indicating whether to use theme_gray() or theme_minimal(). Default is TRUE.
#'      \item legend   :  logical, indicating whether to show the legend. Default is TRUE.
#' }    
#'      
#' @export
#'     
#' @examples
#' # make some fake data
mk_heatmap = function(df) {
        function(xvar, yvar, fillby, xlab="", ylab="", main="", base_size = 12, 
                 use_theme_gray=T, legend=T) {
                p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar))
                
                if (use_theme_gray)
                        p = p + ggplot2::theme_gray(base_size=base_size) 
                else 
                        p = p + ggplot2::theme_minimal(base_size=base_size)
                
                p = p + ggplot2::geom_tile(ggplot2::aes_string(fill=fillby), color="white") + 
                        ggplot2::scale_fill_gradient(low="white", high="steelblue") +
                        ggplot2::labs(x=xlab, y=ylab, title=main) +
                        ggplot2::scale_x_discrete(expand = c(0, 0)) +
                        ggplot2::scale_y_discrete(expand = c(0, 0)) +
                        ggplot2::theme(axis.ticks = ggplot2::element_blank())
                                
                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                
                p
        }
}

