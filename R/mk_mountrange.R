#' @title Create a function for making publishable ggplot2 sets of density plots
#' that give the impression of a mountatin range in order to show changes in
#' distributions over time/space or within different groups.
#'
#' @description
#' \code{mk_mountrange} takes a data frame as input and returns a function for
#' making a set of density plots of any continuous variable (x) from the data
#' frame for each level of a categorical variable (y).
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar = "1", fillby = '..x..', cut_tail = 0.005,
#'                scale_height = 1, font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item yvar. String, name of a categorical variable for y-axis. It's used
#'      to group x. Default is "1", meaning no groups.
#'      \item fillby. String, name of the statistic for filling the space under
#'      the density curves. Possible values are 'x' (default), 'density' (pdf),
#'      'ndensity' (normalized pdf with max = 1), and 'ecdf'.
#'      \item cut_tail. A number between 0 and 1. Lines with heights below this
#'      cutoff will be removed. The cutoff is relative to the overall maximum,
#'      so cut_tail = 0.01 would remove everything that is 1 percent or less
#'      than the highest point. Default = 0.005, so 0.5 percent or less are removed.
#'      \item scale_height. Number, default = 3. scale_height=1 means the tallest
#'      density curve just touches the baseline of the next higher one. Smaller
#'      values create a separation between the curves, and larger values create
#'      more overlap.
#'      \item color_direction. 1 or -1. Sets the order of colors. If 1 (default),
#'      colors are ordered from darkest to lightest. If -1, ordered from lightest
#'      to darkest.
#'      \item hue. A string indicating the colormap option to use. Four options
#'      are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C",
#'      the default option), "viridis" (or "D") and "cividis" (or "E"). Usually,
#'      you want to use "C" (reddish warm/hot) or 'D' (greenish cool/cold).
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_mountrange.R
mk_mountrange = function(df) {
        function(xvar, yvar, fillby = 'x', cut_tail = 0.005, scale_height = 3,
                 color_direction = 1, hue = 'C', font_size = 14) {

                # --- Prep --- #

                if (any(class(df[[yvar]]) %in% c("integer", "numeric")))
                        stop(paste("The y variable,", paste0(yvar, ","),
                                   "is integer or numeric. Change it to factor or character first."))

                legend_title = switch(fillby,
                                      x = xvar,
                                      density = 'Density',
                                      ndensity = 'Normalized density',
                                      ecdf = 'Tail probability')

                # --- Main Plot --- #

                p = ggplot(df, aes(!!as.name(xvar), !!as.name(yvar)))
                if (fillby == 'ecdf')
                        p = p + aes(fill = 0.5 - abs(0.5 - ..ecdf..))
                if (fillby == 'x')
                        p = p + aes(fill = ..x..)
                if (fillby == 'density')
                        p = p + aes(fill = ..density..)
                if (fillby == 'ndensity')
                        p = p + aes(fill = ..ndensity..)

                p = p + ggridges::geom_density_ridges_gradient(
                                scale = scale_height, rel_min_height = cut_tail,
                                calc_ecdf = TRUE) +
                        scale_fill_viridis_c(name = legend_title, option = hue,
                                             direction = color_direction) +
                        # # expand the lower limit of x by 0, upper limit by 0
                        # scale_x_continuous(expand = c(0, 0)) +
                        # expand the lower limit of y by 0.05, upper limit by 0
                        scale_y_discrete(expand = c(0.05, 0))


                # --- Customize Theme --- #

                p + labs(x = xvar, y = NULL) + theme_cowplot(font_size)
        }
}

