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
#' \code{function(xvar, yvar, cut_tail = 0.005, font_size = 14,
#'                xlab = xvar, ylab = NULL, ...)}
#' \itemize{
#'      \item xvar : string, name of a continuous variable for x-axis.
#'      \item yvar : string, name of a categorical variable for y-axis. It's
#'                   used to group x. Its type must be character or factor.
#'      \item cut_tail : a number between 0 and 1. Lines with heights below this
#'                       cutoff will be removed. The cutoff is relative to the
#'                       overall maximum, so cut_tail = 0.01 would remove
#'                       everything that is 1% or less than the highest point.
#'                       Default = 0.005, so 0.5% or below are removed.
#'      \item font_size: overall font size. Default = 14. The font size of the
#'                       axes and legend text is a fraction of this value.
#'      \item xlab     : string, the x-axis label. Default is xvar.
#'      \item ylab     : string, the y-axis label. Default is NULL.
#'      \item ...      : other arguments for ggplot2::labs(), for example,
#'                       title, subtitle, caption and etc.
#' }
#' @export
#' @examples
#' library(ezplot)
#' plt = mk_densityplot(iris)
#' plt("Sepal.Length", "Species")
#' plt("Sepal.Length", "Species", font_size = 9)
#'
#' plt = mk_densityplot(films)
#' plt("rating", "year_cat", subtitle = "Density Plot")
#' plt("rating", "year") # throws error when yvar is integer or numeric
#'
#' p = plt("boxoffice", "year_cat")
#' scale_axis(p, "x", scale = "log10")
#'
#' p = plt("bo_bt_ratio", "year_cat", font_size = 10)
#' scale_axis(p, "x", scale = "log10")
#' p = plt("bo_bt_ratio", "year_cat", cut_tail = 10^-4)
#' scale_axis(p, "x", scale = "log10")
#' p = plt("bo_bt_ratio", "year_cat", cut_tail = 10^-1.65, xlab = "Boxoffice / Budget Ratio")
#' scale_axis(p, "x", scale = "log10")
mk_densityplot = function(df) {
        function(xvar, yvar, cut_tail = 0.005, font_size = 14,
                 xlab = xvar, ylab = NULL, ...) {

                # --- Prep --- #

                if (any(class(df[[yvar]]) %in% c("integer", "numeric")))
                        stop(paste("The y variable,", paste0(yvar, ","),
                                   "is integer or numeric. Change to factor or character."))

                # --- Main Plot --- #

                p = ggplot2::ggplot(
                        df, ggplot2::aes_string(xvar, yvar, group = yvar)) +
                        ggridges::stat_density_ridges(
                                ggplot2::aes(fill = 0.5 - abs(0.5 - ..ecdf..)),
                                geom = "density_ridges_gradient", calc_ecdf = T,
                                rel_min_height = cut_tail, scale = 1) +
                        ggplot2::scale_fill_viridis_c(direction = -1) +
                        # # expand the lower limit of x by 0, upper limit by 0
                        # ggplot2::scale_x_continuous(expand = c(0, 0)) +
                        # expand the lower limit of y by 0.05, upper limit by 0
                        ggplot2::scale_y_discrete(expand = c(0.05, 0))


                # --- Format Legend --- #

                # remove legend
                p = p + ggplot2::guides(color = FALSE, fill = FALSE)


                # --- Customize Theme --- #

                p + ggplot2::labs(x = xlab, y = ylab, ...) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                aspect.ratio = 1,

                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()
                                )
        }
}

