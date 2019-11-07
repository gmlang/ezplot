#' @title Create a function for drawing publishable ggplot2 CDF and log10(CCDF)
#' plots side by side on one canvas. The resulting figure is used to test if
#' the sample distribution is exponential.
#'
#' @description
#' \code{test_expdist} takes a data frame as input and returns a function for
#' drawing CDF plot and log10(CCDF) plot of any continuous variable
#' from the data frame. CCDF standands for Complement CDF. If log10(CCDF) is
#' linear, the sample data are exponentially distributed.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, title_left, title_right, xlab = varname, ...)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its complement CDF.
#'      \item title_left. String, title of the left figure.
#'      \item title_right. String, title of the right figure.
#'      \item xlab. String, x label of both left and right figures. Default is varname.
#'      \item .... Other parameters for making a CDF plot. A common one, for
#'      example, is `add_vline_median = TRUE`, which will add a vertical line at
#'      the median. Another common one is `show_label_median = FALSE`, which
#'      will suppress the display of median value along the median vline. See
#'      \code{\link{mk_cdfplot}} for a full list of parameters.
#' }
#'
#' @export
#' @examples inst/examples/ex-test_expdist.R
test_expdist = function(df) {

        draw_cdf = mk_cdfplot(df)

        function(varname, title_left, title_right, xlab = varname, ...) {

                if (missing(title_left)) {
                        tit1 = paste("Cumulative Distribution of", varname)
                } else {
                        tit1 = title_left
                }

                if (missing(title_right)) {
                        tit2 = paste("Is", varname, "exponentially distributed?")
                } else {
                        tit2 = title_right
                }

                p1 = draw_cdf(varname, legend_pos = 'top', ...) %>%
                        square_fig() %>%
                        add_labs(title = tit1, xlab = xlab)

                p2 = draw_cdf(varname, complement = TRUE, legend_pos = 'top',
                              ...) %>%
                        scale_axis(scale = 'log10') %>%
                        square_fig() %>%
                        add_labs(title = tit2, xlab = xlab)

                cowplot::plot_grid(p1, p2)
        }
}
