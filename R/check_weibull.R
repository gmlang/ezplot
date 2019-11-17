#' @title Check if observed data can be modeled by the weibull distribution.
#'
#' @description
#' The weibull distribution is a generalization of the exponential distribution
#' It's often used to study "time-to-failure", where it gives a failure rate
#' proportional to a power of time (see https://en.wikipedia.org/wiki/Weibull_distribution).
#' \code{check_weibull} takes in a data frame and returns a function for making
#' a CDF plot and a variant of CCDF plot on a log-log scale side by side on one
#' canvas of any continuous variable from the data frame. CCDF standands for
#' Complement CDF. If the -logCCDF on a log-log scale is a straight line, the
#' observed data follows a weibull distribution with the shape parameter
#' k = slope and the location parameter lambda = e^(-intercept / k).
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
#' @seealso \code{\link{est_params_weibull}} for how the parameters of the
#' theoretical weibull distribution are estimated.
#' @export
#' @examples inst/examples/ex-check_weibull.R
check_weibull = function(df) {

        draw_cdf = mk_cdfplot(df)

        function(varname, linew = 0.7, xlab = varname, title_left, title_right,
                 subtitle_left, subtitle_right, caption_left, caption_right,
                 digits = 2, ...) {

                # --- prep data --- #

                params = est_params_weibull(df, varname, digits)
                slope = params['shape']

                # --- prep args and fig elements --- #

                if (missing(title_left)) {
                        tit1 = paste("Cumulative Distribution of", varname)
                } else {
                        tit1 = title_left
                }

                if (missing(title_right)) {
                        tit2 = paste("Is", varname, "weibull?")
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
                                'The smooth curve is from weibull(shape = ',
                                params['shape'], ', location = ',
                                params['location'], ').')
                } else {
                        subtit1 = subtitle_left
                }

                if (missing(subtitle_right)) {
                        subtit2 = paste0(
                                'If the trend is linear, the data can be modeled by the weibull\n',
                                'distribution with shape = ', params['shape'],
                                ' and location = ', params['location'], '.')
                } else {
                        subtit2 = subtitle_right
                }

                slope_label = paste0('Slope: ', slope)
                slope_label_pos = setNames(
                        c(log(pretty(df[[varname]], 6))[3], 1), c('x', 'y'))

                # --- left figure --- #

                p1 = draw_cdf(varname, legend_pos = 'top', linew = linew,
                              pad = FALSE, ...)

                # extract data behind p1 to be used for making the right figure
                d = layer_data(p1)

                # add curve from model
                p1 = p1 + stat_function(fun = pweibull,
                                        args = list(shape = params['shape'],
                                                    scale = params['location']),
                                        alpha = 0.8, size = linew,
                                        # color-blind friendly orange
                                        color = '#F28E2B')
                # don't change x scale, only divide x-axis to 6 ticks
                p1 = scale_axis(p1, 'x', nticks = 6)
                p1 = add_labs(p1, xlab = xlab, title = tit1,
                              subtitle = subtit1, caption = cap1)


                # --- right figure --- #

                p2 = ggplot(d, aes(x, -log(1-y))) +
                        geom_step(size = linew, alpha = 0.8)
                p2 = p2 %>% # apply log scale on y-axis, divide y-axis to 6 ticks
                        scale_axis(scale = 'log', nticks = 6) %>%
                        # apply log scale on x-axis, divide x-axis to 6 ticks
                        scale_axis('x', scale = 'log', nticks = 6) %>%
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

                # customize theme
                if (exists('font_size')) {
                        font_size = list(...)[['font_size']]
                } else {
                        font_size = 14
                }
                p2 = p2 + labs(x = xlab, y = '-logCCDF') +
                        theme_cowplot(font_size)

                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
