#' @title Test if some observed data are normally distributed.
#'
#' @description
#' The normal distribution is the most commonly used probability distribution.
#' Many real world data such as height can be modeled via a normal distribution.
#' \code{test_normality} takes in a data frame and returns a function for making
#' ggplot2 type of CDF and the standard normal probability plots side by side
#' on one canvas of any continuous variable from the data frame. If normal
#' probability plot is linear, the observed variable is normally distributed
#' with mean and standard deviation equal to the sample mean and sample standard
#' deviation.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, linew = 0.7, xlab_left = varname, title_left,
#'                title_right, subtitle_left, subtitle_right,
#'                caption_left, caption_right, xscale_left, ...)}
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
#'      \item xscale_left. String, scale of x-axis of the left figure. Possible
#'      values are described at \code{\link{scale_axis}}. There's no parameter
#'      to change the x-scale of the right figure because the x-axis of the
#'      standard normal probability plot always goes from -3 to +3 with 1
#'      increment at a time.
#' }
#'
#' @export
#' @examples inst/examples/ex-test_normality.R
test_normality = function(df) {
        function(varname, linew = 0.7, xlab_left = varname, title_left,
                 title_right, subtitle_left, subtitle_right,
                 caption_left, caption_right, xscale_left, ...) {

                # --- prep data --- #

                # calc sample avg and std
                avg = round(mean(df[[varname]], na.rm = T), 3)
                std = round(sd(df[[varname]], na.rm = T), 3)

                # add standardized version of the variable to be plotted since
                # we'll always want to compare to standard normal distribution
                # on the Q-Q normal plot
                stand_varname = paste0('standard_', varname)
                df[[stand_varname]] = (df[[varname]] - avg) / std

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
                                'If data points are scattered along the line and within the\n',
                                'confidence band, the data can be approximated by the normal\n',
                                'distribution with mean = ', avg, ' and standard deviation = ',
                                std, '.')
                } else {
                        subtit2 = subtitle_right
                }

                # --- left figure --- #

                p1 = draw_cdf(varname, legend_pos = 'top', linew = linew, ...) +
                        stat_function(fun = pnorm,
                                      args = list(mean = avg, sd = std),
                                      alpha = 0.8, size = linew,
                                      # color-blind friendly orange
                                      color = '#F28E2B')
                p1 = add_labs(p1, xlab = xlab_left, title = tit1,
                              subtitle = subtit1, caption = cap1)
                if (missing(xscale_left)) {
                        # don't change x scale, only divide x-axis to 8 ticks
                        p1 = scale_axis(p1, 'x', nticks = 8)
                } else {
                        p1 = scale_axis(p1, 'x', scale = xscale_left, nticks=8)
                }


                # --- right figure --- #
                if (!exists('font_size')) font_size = 14
                p2 = draw_qqplot(stand_varname, font_size = font_size) %>%
                        add_labs(title = tit2, subtitle = subtit2, caption = cap2)

                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
