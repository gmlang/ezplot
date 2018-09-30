#' @title Create a function for making publishable ggplot2 density plots to
#' show distributions over time/space or within different groups.
#'
#' @description
#' \code{mk_densityplot} takes a data frame as input and returns a function for
#' making density plots of any continuous variable (x) from the data frame for
#' each group of a categorical variable (y).
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar = "1", cut_tail = 0.005, font_size = 14)}
#' \itemize{
#'      \item xvar : string, name of a continuous variable for x-axis.
#'      \item yvar : string, name of a categorical variable for y-axis. It's
#'                   used to group x. Default is "1", meaning no groups.
#'      \item cut_tail : a number between 0 and 1. Lines with heights below this
#'                       cutoff will be removed. The cutoff is relative to the
#'                       overall maximum, so cut_tail = 0.01 would remove
#'                       everything that is 1% or less than the highest point.
#'                       Default = 0.005, so 0.5% or below are removed.
#'      \item font_size: overall font size. Default = 14. The font size of the
#'                       axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_densityplot.R
mk_densityplot = function(df) {
        function(xvar, yvar = "1", cut_tail = 0.005, font_size = 14) {

                # --- Prep --- #

                if (any(class(df[[yvar]]) %in% c("integer", "numeric")))
                        stop(paste("The y variable,", paste0(yvar, ","),
                                   "is integer or numeric. Change to factor or character."))

                # --- Main Plot --- #

                p = ggplot(
                        df, aes_string(xvar, yvar, group = yvar)) +
                        ggridges::stat_density_ridges(
                                aes(fill = 0.5 - abs(0.5 - ..ecdf..)),
                                geom = "density_ridges_gradient", calc_ecdf = T,
                                rel_min_height = cut_tail, scale = 1) +
                        scale_fill_viridis_c(direction = -1) +
                        # # expand the lower limit of x by 0, upper limit by 0
                        # scale_x_continuous(expand = c(0, 0)) +
                        # expand the lower limit of y by 0.05, upper limit by 0
                        scale_y_discrete(expand = c(0.05, 0))


                # --- Format Legend --- #

                # remove legend
                p = p + guides(color = FALSE, fill = FALSE)


                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_cowplot(font_size)
        }
}

