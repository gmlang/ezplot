#' @title Create a function for making publishable ggplot2 percentile plots of
#' one continuous variable against another continuous variable.
#'
#' @description
#' \code{mk_percentileplot_convars} takes a data frame as input and returns a
#' function for binning a continuous variable from the data frame, and plotting
#' the average values of the bins on the x-axis and the (25th, 50th and 75th)
#' percentiles of another continuous variable grouped by the bins on the y axis.
#' This is a good alternative to scatterplots when the data points overlap.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, pt_size = 1, linew = 0.7, xcuts = 15,
#'                probs = c(0.75, 0.5, 0.25),
#'                legend_title = c('75th', '50th', '25th'), legend_pos = "right",
#'                font_size = 14)}
#' \itemize{
#'      \item xvar. String, name of a continuous variable for x-axis.
#'      \item yvar. String, name of a continuous variable for y-axis.
#'      \item pt_size. Number, size of the points. Default = 1.
#'      \item linew. Number, width of the line. Default = 0.7.
#'      \item xcuts. A numeric vector of two or more unique cut points or
#'      a single number (greater than or equal to 2) giving the number of
#'      intervals into which x is to be cut. Default = 15.
#'      \item probs. Numeric vector. Probabilities for which the percentiles will
#'      be calculated and plotted. Default = c(0.75, 0.5, 0.25), and it'll plot
#'      the 75th, 50th and 25th percentiles of the y variable at each x value.
#'      \item legend_title. String, legend title.
#'      Default = = c('75th', '50th', '25th').
#'      \item legend_pos. String, legend position. Default = "right".
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_percentileplot_convars.R
mk_percentileplot_convars = function(df) {
        function(xvar, yvar, pt_size = 1, linew = 0.7,
                 xcuts = 15, probs = c(0.75, 0.5, 0.25),
                 legend_title = c('75th', '50th', '25th'), legend_pos = "right",
                 font_size = 14) {

                # --- Prep  --- #

                dat = na.omit(df[c(xvar, yvar)])
                xbin_idx = cut(dat[[xvar]], xcuts, include.lowest=T, right = F)
                dat = dplyr::bind_rows(
                        lapply(split(dat, xbin_idx),
                               function(elt) setNames(
                                       data.frame(mean(elt[[xvar]]),
                                                  rbind(quantile(elt[[yvar]],
                                                                 probs))
                                                  ),
                                       c(xvar, paste0(probs * 100, 'th')))
                               )
                        )
                dat = tidyr::gather(dat, key = 'nth_percent',
                                    value = !!as.name(yvar), -!!as.name(xvar))


                # --- Main Plot --- #

                p = ggplot(dat, aes_string(xvar, yvar, color = 'nth_percent')) +
                        geom_line(aes_string(group = 'nth_percent'),
                                  size = linew, alpha = 0.8) +
                        geom_point(size = pt_size, alpha = 0.8)
                # print(p)

                # break y, x-axis into 10 pieces
                ybreaks = pretty(dat[[yvar]], n = 10)
                xbreaks = pretty(dat[[xvar]], n = 10)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks)) +
                        scale_x_continuous(breaks = xbreaks,
                                           limits = range(xbreaks))

                # --- Customize Theme --- #

                p = p + labs(x = xvar, y = yvar) + theme_cowplot(font_size)


                # --- Format Legend --- #

                p = p + ggthemes::scale_color_tableau("Color Blind",
                                                      direction = -1,
                                                      name = legend_title) +
                        # sort legend labels in descending order (from Z to A)
                        guides(color = guide_legend(reverse = TRUE)) +
                        theme(legend.position = legend_pos)
                p

        }
}
