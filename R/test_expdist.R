#' @title Test if some observed data are exponentially distributed.
#'
#' @description
#' The exponential distribution is the probability distribution of the time
#' between events in a process in which events occur continuously and
#' independently at a constant average rate. Its mean and standard deviation
#' are equal to the inverse of the average rate.
#' \code{test_expdist} takes in a data frame and returns a function for making
#' ggplot2 type of CDF and CCDF (on log-y scale) plots side by side on one
#' canvas of any continuous variable from the data frame. CCDF standands for
#' Complement CDF. If CCDF on log-y scale is linear, the observed variable can
#' be modeled by an exponential distribution with a rate equal to the absolute
#' value of the slope.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, linew = 0.7, xlab = varname, title_left, title_right,
#'                subtitle_left, subtitle_right, caption_left, caption_right,
#'                digits = 2, ...)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its complement CDF.
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item xlab. String, x label of the left and the right figures.
#'      Default is varname.
#'      \item title_left. String, title of the left figure.
#'      \item title_right. String, title of the right figure.
#'      \item subtitle_left. String, subtitle of the left figure.
#'      \item subtitle_right. String, subtitle of the right figure.
#'      \item caption_left. String, caption of the left figure.
#'      \item caption_right. String, caption of the right figure.
#'      \item digits. Integer, the number of digits after the decimal point
#'      for the estimated parameter values of the theoretical distribution.
#'      Default = 2.
#'      \item .... Other parameters for making a CDF plot. A common one, for
#'      example, is `add_vline_median = TRUE`, which will add a vertical line at
#'      the median. Another common one is `show_label_median = FALSE`, which
#'      will suppress the display of median value along the median vline. See
#'      \code{\link{mk_cdfplot}} for a full list of parameters.
#' }
#'
#' @seealso \code{\link{est_params_expdist}} for how the rate of the theoretical
#' exponential distribution is estimated.
#' @export
#' @examples inst/examples/ex-test_expdist.R
test_expdist = function(df) {

        draw_cdf = mk_cdfplot(df)

        function(varname, linew = 0.7, xlab = varname, title_left, title_right,
                 subtitle_left, subtitle_right, caption_left, caption_right,
                 digits = 2, ...) {

                # --- prep data --- #

                rate = est_params_expdist(df, varname, digits = digits)
                slope = -1 * rate

                # --- prep args and fig elements --- #

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

                if (missing(subtitle_left)) {
                        subtit1 = paste0(
                                'The stepwise curve is from the data.\n',
                                'The smooth curve is from Exp(', rate, ').')
                } else {
                        subtit1 = subtitle_left
                }

                if (missing(subtitle_right)) {
                        subtit2 = paste0(
                                'If the trend is linear, the data can be ',
                                'approximated by \nthe exponential ',
                                'distribution with rate = ', rate, '.')
                } else {
                        subtit2 = subtitle_right
                }

                slope_label = paste('Slope:', slope)
                slope_label_pos = setNames(c(pretty(df[[varname]], 8)[5], 1),
                                           c('x', 'y'))

                # --- left figure --- #

                p1 = draw_cdf(varname, legend_pos = 'top', linew = linew,
                              pad = FALSE, ...)
                # add curve from model
                p1 = p1 + stat_function(fun = pexp,
                                        args = list(rate = rate),
                                        alpha = 0.8, size = linew,
                                        # color-blind friendly orange
                                        color = '#F28E2B')
                # don't change x scale, only divide x-axis to 8 ticks
                p1 = scale_axis(p1, 'x', nticks = 8)
                p1 = add_labs(p1, xlab = xlab, title = tit1,
                              subtitle = subtit1, caption = cap1)


                # --- right figure --- #

                # don't add median vline for CCDF plot regardless user input
                if (exists('add_vline_median')) add_vline_median = FALSE
                p2 = draw_cdf(varname, complement = TRUE, legend_pos = 'top',
                              linew = linew, pad = FALSE, ...) %>%
                        # apply log scale on y-axis, divide y-axis to 8 ticks
                        scale_axis(scale = 'log', nticks = 6) %>%
                        # don't change x scale, only divide x-axis to 8 ticks
                        scale_axis('x', nticks = 8) %>%
                        add_labs(xlab = xlab, title = tit2, subtitle = subtit2,
                                 caption = cap2)
                # add line to help the eye to detect linear trend
                p2 = p2 + geom_line(stat = "smooth", method = "lm",
                                    alpha = 0.8, size = linew,
                                    linetype = 'longdash',
                                    # color-blind friendly orange
                                    color = '#F28E2B') +
                        # add text label to show slope value
                        annotate("text", label = slope_label,
                                 x = slope_label_pos['x'],
                                 y = slope_label_pos['y'], size = 5)

                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
