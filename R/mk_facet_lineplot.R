#' @title Create a function for making publishable ggplot2 faceted plot.
#'
#' @description
#' \code{mk_facet_lineplot} takes a data frame as input and returns a function for
#' making faceted plots that can display relationship of upto 5 variables at
#' one time.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, xvar_top, yvar_rt, gpby = "1", ylab_rt = NULL,
#'                legend_title = NULL, palette = "D", linew = 0.7,
#'                font_size = 14)}
#' \itemize{
#'      \item xvar    : string, name of a continuous var for the bottom x-axis.
#'      \item yvar    : string, name of a continuous var for the left y-axis.
#'      \item xvar_top: string, name of a categorical var for the top x-axis.
#'      \item yvar_rt : string, name of a continuous var for the right y-axis.
#'      \item gpby    : string, name of a continuous or categorical variable for
#'                      coloring the curves. Default = "1", meaning no such
#'                      variable is supplied.
#'      \item ylab_rt : string, 2nd y-axis label. If NULL (default), use the
#'                      value of yvar_rt.
#'      \item legend_title: string, legend title. If NULL (default), use the
#'                      value of gpby.
#'      \item palette : string, the colormap option to use. Possible values are
#'                      "A", "B", "C", "D" (default) and "E".
#'      \item linew   : number, width of the line. Default = 0.7.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_facet_lineplot.R
mk_facet_lineplot = function(df) {
        function(xvar, yvar, xvar_top, yvar_rt, gpby = "1", ylab_rt = NULL,
                 legend_title = NULL, palette = "D", linew = 0.7,
                 font_size = 14) {

                # --- Main Plot --- #

                p = ggplot(df, aes_string(x = xvar, y = yvar,
                                          group = gpby, color = gpby)) +
                        facet_grid(reformulate(xvar_top, yvar_rt)) +
                        geom_hline(yintercept = 0.8, size = linew, alpha = 0.8,
                                   linetype = "dashed") +
                        geom_line(size = linew, alpha = 0.8)

                # add 2nd y-axis
                if (is.null(ylab_rt)) ylab_rt = yvar_rt
                p = p + scale_y_continuous(sec.axis = dup_axis(name = ylab_rt))

                # add curves defined and colored by gpby
                if (class(df[[gpby]]) %in% c("character", "factor")) {
                        p = p + scale_color_viridis_d(name = gpby,
                                                      option = palette,
                                                      direction = -1)
                } else {
                        p = p + scale_color_viridis_c(name = gpby,
                                                      option = palette,
                                                      direction = -1)
                }



                # --- Format Legend --- #

                if (gpby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else {
                        if (is.null(legend_title)) legend_title = gpby
                        p = p + guides(color = guide_legend(title = legend_title))
                }

                # --- Customize Theme --- #

                p + labs(subtitle = xvar_top) +
                        theme_gray(font_size) +
                        theme(axis.text.y.right = element_blank(),
                              axis.ticks.length = unit(0, "cm"),
                              plot.subtitle = element_text(hjust = 0.5)
                              )
        }
}



