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
#' \code{function(varname, xlab_left = varname, title_left, title_right,
#'                caption_left, caption_right, font_size = 14)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its normal probability plot.
#'      \item xlab_left. String, x label of the left figure. Default is varname.
#'      \item title_left. String, title of the left figure.
#'      \item title_right. String, title of the right figure.
#'      \item caption_left. String, caption of the left figure.
#'      \item caption_right. String, caption of the right figure.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value. It controls both plots.
#' }
#'
#' @export
#' @examples inst/examples/ex-test_normality.R
test_normality = function(df) {
        function(varname, xlab_left = varname, title_left, title_right,
                 caption_left, caption_right, font_size = 14) {

                # --- prep --- #

                # add standardized version of the variable to be plotted since
                # we'll always want to compare to standard normal distribution
                # on the Q-Q normal plot
                stand_varname = paste0('standard_', varname)
                df[[stand_varname]] =
                        (df[[varname]] - mean(df[[varname]], na.rm = T)) /
                                sd(df[[varname]], na.rm = T)

                # make functions for plotting
                draw_cdf = mk_cdfplot(df)
                draw_qqplot = mk_qqplot(df)

                # prep figure annotations
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

                p1 = draw_cdf(varname, legend_pos = "top",
                              add_vline_median = TRUE,
                              font_size = font_size) %>%
                        square_fig() %>%
                        add_labs(xlab = xlab_left, title = tit1, caption = cap1)

                p2 = draw_qqplot(stand_varname, font_size = font_size) %>%
                        add_labs(title = tit2, caption = cap2)

                cowplot::plot_grid(p1, p2)
        }
}
