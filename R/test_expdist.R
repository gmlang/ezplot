#' @title Create a function for drawing publishable ggplot2 CDF and log10(CCDF)
#' plots side by side on one canvas. The resulting figure is used to test if
#' the distribution of the data is exponential.
#'
#' @description
#' \code{test_expdist} takes a data frame as input and returns a function for
#' drawing CDF plot and log10(CCDF) plot of any continuous variable
#' from the data frame. CCDF standands for Complement CDF. If log10(CCDF) is
#' linear, the observed data are exponentially distributed.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, xlab = varname, title_left, title_right,
#'                caption_left, caption_right, xscale_left, ...)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its complement CDF.
#'      \item xlab. String, x label of the left and the right figures.
#'      Default is varname.
#'      \item title_left. String, title of the left figure.
#'      \item title_right. String, title of the right figure.
#'      \item caption_left. String, caption of the left figure.
#'      \item caption_right. String, caption of the right figure.
#'      \item xscale_left. String, scale of x-axis of the left figure. Possible
#'      values are described at \code{\link{scale_axis}}. There's no parameter
#'      to change the x-scale of the right figure because we need to keep the
#'      x-axis of CCDF at the original/raw scale in order to detect if the
#'      distribution is exponential or not.
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

        function(varname, xlab = varname, title_left, title_right,
                 caption_left, caption_right, xscale_left, ...) {

                # --- prep --- #

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

                if (missing(caption_left)) {
                        cap1 = NULL
                } else {
                        cap1 = caption_left
                }

                if (missing(caption_right)) {
                        cap2 = NULL
                } else {
                        cap2 = caption_right
                }

                # --- main --- #

                p1 = draw_cdf(varname, legend_pos = 'top', ...) %>%
                        add_labs(xlab = xlab, title = tit1, caption = cap1)

                p2 = draw_cdf(varname, complement = TRUE, legend_pos = 'top',
                              ...) %>%
                        scale_axis(scale = 'log10') %>%
                        add_labs(xlab = xlab, title = tit2, caption = cap2)

                if (!missing(xscale_left))
                        p1 = scale_axis(p1, 'x', scale = xscale_left, nticks=8)


                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
