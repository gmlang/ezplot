#' @title Check if observed data can be modeled by the normal distribution.
#'
#' @description
#' The normal distribution is the most commonly used probability distribution.
#' Many real world data such as height can be modeled via the normal distribution.
#' \code{check_normality} takes in a data frame and returns a function for making
#' ggplot2 type of CDF plot and the normal probability plot side by side
#' on one canvas of any continuous variable from the data frame. If normal
#' probability plot is linear, the observed data can be modeled by the normal
#' distribution with mean and standard deviation equal to the sample mean and
#' standard deviation of the observed data.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, linew = 0.7, xlab_left = varname, title_left,
#'                title_right, subtitle_left, subtitle_right,
#'                caption_left, caption_right, digits = 2, ...)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. Its empirical CDF
#'      will be plotted along side its normal probability plot.
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item xlab_left. String, x label of the left figure. Default is varname.
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
#' @export
#' @examples inst/examples/ex-check_normality.R
check_normality = function(df) {
        function(varname, linew = 0.7, xlab_left = varname, title_left,
                 title_right, subtitle_left, subtitle_right,
                 caption_left, caption_right, digits = 2, ...) {

                # --- prep data --- #

                # calc sample avg and std
                avg = mean(df[[varname]], na.rm = T)
                std = sd(df[[varname]], na.rm = T)

                # round so that we don't display too many digits after decimal
                avg = round(avg, digits)
                std = round(std, digits)

                # make functions for plotting
                draw_cdf = mk_cdfplot(df)
                draw_qqplot = mk_qqplot(df)

                # --- prep args and fig elements --- #

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

                if (missing(subtitle_left)) {
                        subtit1 = paste0(
                                'The stepwise curve is from the data.\n',
                                'The smooth curve is from Norm(', avg, ', ',
                                std, ').')
                } else {
                        subtit1 = subtitle_left
                }

                if (missing(subtitle_right)) {
                        subtit2 = paste0(
                                'If data points fall along the line and within the confidence band,\n',
                                'the data can be modeled by the normal distribution with\n',
                                'mean = ', avg, ' and standard deviation = ', std, '.')
                } else {
                        subtit2 = subtitle_right
                }

                # --- left figure --- #

                p1 = draw_cdf(varname, legend_pos = 'top', linew = linew,
                              pad = FALSE, ...)
                # add curve from model
                p1 = p1 + stat_function(fun = pnorm,
                                        args = list(mean = avg, sd = std),
                                        alpha = 0.8, size = linew,
                                        # color-blind friendly orange
                                        color = '#F28E2B')
                # don't change x scale, only divide x-axis to 8 ticks
                p1 = scale_axis(p1, 'x', nticks = 8)
                p1 = add_labs(p1, xlab = xlab_left, title = tit1,
                              subtitle = subtit1, caption = cap1)

                # --- right figure --- #

                rest_args = list(...)
                if (!'font_size' %in% names(rest_args)) {
                        font_size = 14
                } else {
                        font_size = rest_args[['font_size']]
                }
                p2 = draw_qqplot(varname, font_size = font_size)
                p2 = add_labs(p2, title = tit2, subtitle = subtit2,
                              caption = cap2)

                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
