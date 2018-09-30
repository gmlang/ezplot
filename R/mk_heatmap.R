#' @title Create a function that draws ggplot2 heat maps.
#
#' @description
<<<<<<< HEAD
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
=======
#' \code{mk_heatmap} takes a data frame as input and returns a function that
#' can be used to make heat maps using variables in the data frame.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
#'                palette = "C", font_size = 8)}
#' \itemize{
#'      \item xvar   : string, name of a categorical variable for x-axis.
#'      \item yvar   : string, name of another categorical variable for y-axis.
#'      \item fillby : string, name of a continuous variable, and its values
#'                     are used to color the tiles.
#'      \item facet_by: string, name of a categorical variable for grouping and
#'                      creating facets. Default = NULL.
#'      \item facet_ncol: number of columns when facetting. Default = 1.
#'                        Only works when facet_by is not NULL.
#'      \item palette   : string, the colormap option to use. Possible values
#'                        are "A", "B", "C" (default), "D" and "E".
#'      \item font_size : overall font size. Default = 8. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_heatmap.R
mk_heatmap = function(df) {
        function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
                 palette = "C", font_size = 8) {

                # --- Main Plot --- #

                p = ggplot(df, aes_string(x = xvar, y = yvar, fill = fillby)) +
                        geom_tile(color = "white", size = 0.1) +
                        scale_fill_viridis_c(
                                name = fillby, label = scales::comma,
                                option = palette, direction = -1) +
                        coord_equal()

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + facet_wrap(vars(!!as.name(facet_by)),
                                           ncol = facet_ncol)

                # --- Customize Theme --- #

                p + labs(x = NULL, y = NULL) + theme_no_xyaxes(font_size)

>>>>>>> v1.0.0
        }
}

