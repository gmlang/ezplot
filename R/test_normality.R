#' @title Create a function for drawing publishable ggplot2 CDF plot and normal
#' probability plot (also called Q-Q normal plot) side by side on one canvas.
#' The resulting figure is often used to test normality of the sample distribution.
#'
#' @description
#' \code{test_normality} takes a data frame as input and returns a function for
#' drawing CDF plot and normal probability plot of any continuous variable
#' from the data frame.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, detrend = TRUE, title_left, title_right,
#'                xlab_left = varname, ...)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its normal probability plot.
#'      \item detrend. Logical (default = TRUE), If TRUE, the normal probability
#'      plot will be detrended according to the reference Q-Q line. As a result,
#'      if the data points are scattered around the horizontal line y = 0, then
#'      we can conclude the data are approximately normal. The detrend
#'      procedure was described by Thode (2002), and it reduces visual bias
#'      caused by orthogonal distances from Q-Q points to the reference line.
#'      \item title_left. String, title of the left figure.
#'      \item title_right. String, title of the right figure.
#'      \item xlab_left. String, x label of the left figure. Default is varname.
#'      \item .... Other parameters for making a CDF plot. A common one, for
#'      example, is `add_vline_median = TRUE`, which will add a vertical line at
#'      the median. Another common one is `show_label_median = FALSE`, which
#'      will suppress the display of median value along the median vline. See
#'      \code{\link{mk_cdfplot}} for a full list of parameters.
#' }
#'
#' @export
#' @examples inst/examples/ex-test_normality.R
test_normality = function(df) {

        draw_cdf = mk_cdfplot(df)
        draw_qqplot = mk_qqplot(df)

        function(varname, detrend = TRUE, title_left, title_right,
                 xlab_left = varname, ...) {

                if (missing(title_left)) {
                        tit1 = paste("Cumulative Distribution of", varname)
                } else {
                        tit1 = title_left
                }

                if (missing(title_right)) {
                        tit2 = paste("Is", varname, "normally distributed?")
                } else {
                        tit2 = title_right
                }

                p1 = draw_cdf(varname, legend_pos = "top", ...) %>%
                        square_fig() %>%
                        add_labs(title = tit1, xlab = xlab_left)

                p2 = draw_qqplot(varname, detrend = detrend) %>%
                        square_fig() %>%
                        add_labs(title = tit2)

                cowplot::plot_grid(p1, p2)
        }
}
