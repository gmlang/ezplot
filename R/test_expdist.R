#' @title Create a function for drawing publishable ggplot2 CDF and CCDF (on
#' log-y scale) plots side by side on one canvas. The resulting figure is used
#' to test if the data distribution is exponential.
#'
#' @description
#' \code{test_expdist} takes a data frame as input and returns a function for
#' making CDF and CCDF (on log-y scale) plots of any continuous variable
#' from the data frame. CCDF standands for Complement CDF. If CCDF on log-y
#' scale is linear, the observed data are exponentially distributed.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, linew = 0.7, xlab = varname, title_left, title_right,
#'                subtitle_left, subtitle_right, caption_left, caption_right,
#'                xscale_left, ...)}
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

        function(varname, linew = 0.7, xlab = varname, title_left, title_right,
                 subtitle_left, subtitle_right, caption_left, caption_right,
                 xscale_left, ...) {

                # --- prep data --- #

                cdf = get_cdfs(df)(varname)
                xs = df[[varname]]
                ys = log(1 - cdf(xs))
                df_logCCDF = data.frame(x = xs, y = ys) %>%
                        dplyr::filter(is.finite(y))
                fit = lm(y ~ x, data = df_logCCDF)
                slope = round(setNames(coef(fit)['x'], NULL), 4)
                rate = abs(slope)
                avg = round(1 / rate, 2) # mean time between events

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
                                'The smooth curve is from Exp(', rate, '), ',
                                'with mean time between events = ', avg, '.\n',
                                'The stepwise curve is from the data.')
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

                txt = paste('Slope:', slope)
                txt_pos = setNames(c(pretty(xs, 8)[6], 1), c('x', 'y'))

                # --- left figure --- #

                p1 = draw_cdf(varname, legend_pos = 'top', linew = linew, ...) +
                        stat_function(fun = pexp, args = list(rate = rate),
                                      alpha = 0.8, size = linew,
                                      color = '#F28E2B' # color-blind friendly orange
                                      )
                p1 = add_labs(p1, xlab = xlab, title = tit1,
                              subtitle = subtit1, caption = cap1)

                if (missing(xscale_left)) {
                        # don't change x scale, only divide x-axis to 8 ticks
                        p1 = scale_axis(p1, 'x', nticks = 8)
                } else {
                        p1 = scale_axis(p1, 'x', scale = xscale_left, nticks=8)
                }

                # --- right figure --- #

                # don't add median vline for CCDF plot regardless user input
                if (exists('add_vline_median')) add_vline_median = FALSE
                p2 = draw_cdf(varname, complement = TRUE, legend_pos = 'top',
                              linew = linew, ...) %>%
                        # apply log scale on y-axis, divide y-axis to 8 ticks
                        scale_axis(scale = 'log', nticks = 6) %>%
                        # don't change x scale, only divide x-axis to 8 ticks
                        scale_axis('x', nticks = 8) %>%
                        add_labs(xlab = xlab, title = tit2, subtitle = subtit2,
                                 caption = cap2)
                p2 = p2 + ggplot2::geom_line(stat = "smooth", method = "lm",
                                             alpha = 0.8, size = linew,
                                             linetype = 'longdash',
                                             color = '#F28E2B' # color-blind friendly orange
                                             ) +
                        annotate("text", label = txt,
                                 x = txt_pos['x'], y = txt_pos['y'],
                                 size = 5)

                cowplot::plot_grid(square_fig(p1), square_fig(p2))
        }
}
