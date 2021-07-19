#' @title Create a function for making publishable ggplot2 faceted plot.
#'
#' @description
#' \code{mk_facet_lineplot} takes a data frame as input and returns a function for
#' making faceted plots that can display relationship of upto 5 variables at
#' one time.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, xvar_top, yvar_rt, colorby = "1", ylab_rt = NULL,
#'                palette = "D", linew = 0.7, legend_title = colorby,
#'                legend_pos = "right", font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous var for the bottom x-axis.
#'      \item yvar. String, name of a continuous var for the left y-axis.
#'      \item xvar_top. String, name of a categorical var for the top x-axis.
#'      \item yvar_rt. String, name of a continuous var for the right y-axis.
#'      \item colorby. String, name of a continuous or categorical variable for
#'      coloring the curves. Default = "1", meaning no such variable is supplied.
#'      \item ylab_rt. String, 2nd y-axis label. If NULL (default), use the
#'      value of yvar_rt.
#'      \item palette. String, the colormap option to use. Possible values are
#'      "A", "B", "C", "D" (default) and "E".
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item legend_title. String, legend title. Default is the name of the
#'      colorby variable.
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_facet_lineplot.R
mk_facet_lineplot = function(df) {
        function(xvar, yvar, xvar_top, yvar_rt, colorby = "1", ylab_rt = NULL,
                 palette = "D", linew = 0.7, legend_title = colorby,
                 legend_pos = "right", font_size = 14) {

                # --- Main Plot --- #

                p = ggplot(df, aes_string(x = xvar, y = yvar,
                                          group = colorby, color = colorby)) +
                        facet_grid(reformulate(xvar_top, yvar_rt)) +
                        geom_hline(yintercept = 0.8, size = linew, alpha = 0.8,
                                   linetype = "dashed") +
                        geom_line(size = linew, alpha = 0.8)

                # add 2nd y-axis
                if (is.null(ylab_rt)) ylab_rt = yvar_rt
                p = p + scale_y_continuous(sec.axis = dup_axis(name = ylab_rt))

                # add curves defined and colored by colorby
                if (class(df[[colorby]]) %in% c("character", "factor")) {
                        p = p + scale_color_viridis_d(name = legend_title,
                                                      option = palette,
                                                      direction = -1)
                } else {
                        p = p + scale_color_viridis_c(name = legend_title,
                                                      option = palette,
                                                      direction = -1)
                }


                # --- Customize Theme --- #

                p = p + labs(subtitle = xvar_top) +
                        theme_gray(font_size) +
                        theme(axis.text.y.right = element_blank(),
                              axis.ticks.length = unit(0, "cm"),
                              plot.subtitle = element_text(hjust = 0.5),
                              legend.position = legend_pos
                        )


                # --- Format Legend --- #

                if (colorby == "1")  # remove legend
                        p = p + guides(color = 'none', fill = 'none')

                p
        }
}



