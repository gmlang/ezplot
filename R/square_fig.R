#' @title Change the aspect ratio of a plot
#'
#' @description
#' \code{square_fig} takes a ggplot object as input and changes its shape to
#' a square by default. To change the plot to different degrees of rectangular
#' shapes, user can specify an aspect ratio different from 1.
#'
#' @param p           : a ggplot object.
#' @param aspect_ratio: aspect ratio of the panel, Default = 1, which will make
#'                      x and y the same length and hence the figure a square.
#'                      A value > 1 will make y-axis longer than x-axis.
#'                      A value < 1 will make x-axis longer than y-axis.
#' @return a ggplot object of square shape by default or a rectangular
#'         shape when user decides not to use 1 as aspect ratio.
#'
#' @export
#' @examples
#' library(ezplot)
#'
#' # plot boxoffice/budget ratio over the years
#' plt = mk_lineplot(bo_bt_ratio_by_year)
#' p = plt("year", "bo_bt_ratio")
#' p = add_labs(p, xlab = NULL, ylab = "boxoffice/budget ratio", subtitle = NULL,
#'              title = "Boxoffice/Budget Ratio from 1913 to 2014",
#'              caption = "data source: IMBD")
#' square_fig(p)
#' square_fig(p, 2)
#' square_fig(p, 0.5)
square_fig = function(p, aspect_ratio = 1)
        p + ggplot2::theme(aspect.ratio = aspect_ratio)
