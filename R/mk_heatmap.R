#' @title Create a function that draws ggplot2 heat maps.
#
#' @description
#' \code{mk_heatmap} takes a data frame as input and returns a function that
#' can be used to make heat maps using variables in the data frame.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
#'                palette = "C", color_direction = -1, legend_title = fillby,
#'                format_legend_as_comma = FALSE, font_size = 8)}
#' \itemize{
#'      \item xvar. String, name of a categorical variable for x-axis.
#'      \item yvar. String, name of another categorical variable for y-axis.
#'      \item fillby. String, name of a continuous variable, and its values are
#'      used to color the tiles.
#'      \item facet_by. String, name of a categorical variable for grouping and
#'      creating facets. Default = NULL.
#'      \item facet_ncol. Number of columns when facetting. Default = 1. Only
#'      works when facet_by is not NULL.
#'      \item palette. String, the colormap option to use. Possible values are
#'      "A", "B", "C" (default), "D" and "E".
#'      \item color_direction. 1 or -1. Sets the order of colors in the scale.
#'      If 1, colors are ordered from darkest to lightest. If -1 (default),
#'      ordered from lightest to darkest.
#'      \item legend_title. String, legend title. Default uses the name of the
#'      fillby variable.
#'      \item format_legend_as_comma. Logical. If TRUE, display numbers like
#'      2000 as 2,000 on legend. If FALSE (default), display numbers as they are.
#'      \item font_size. Overall font size. Default = 8. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_heatmap.R
mk_heatmap = function(df) {
        function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
                 palette = "C", color_direction = -1, legend_title = fillby,
                 format_legend_as_comma = FALSE, font_size = 8) {

                # --- Main Plot --- #

                p = ggplot(df, aes_string(x = xvar, y = yvar, fill = fillby)) +
                        geom_tile(color = "white", size = 0.1)

                if (format_legend_as_comma) {
                        p = p + scale_fill_viridis_c(
                                name = legend_title, label = scales::comma,
                                option = palette, direction = color_direction)
                } else {
                        p = p + scale_fill_viridis_c(
                                name = legend_title, option = palette,
                                direction = color_direction)
                }

                p = p + coord_equal()

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + facet_wrap(vars(!!as.name(facet_by)),
                                           ncol = facet_ncol)

                # --- Customize Theme --- #

                p + labs(x = NULL, y = NULL) + theme_no_xyaxes(font_size)

        }
}

