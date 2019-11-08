#' @title Create a function for making publishable ggplot2 Q-Q plots.
#'
#' @description
#' \code{mk_qqplot} takes a data frame as input and returns a function for
#' making Q-Q plot of any continuous variable from the data frame.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, dist = "norm", dparams = list(), detrend = TRUE,
#'                ci_band_type = "pointwise", font_size = 14)}
#' \itemize{
#'      \item varname : string, name of a continuous variable. We're interested
#'                      in comparing its empirical distribution with a
#'                      theoretical distribution, for example, the standard
#'                      normal distribution.
#'      \item dist    : string, theoretical probability distribution function to
#'                      compare against. These values are supported: "beta",
#'                      "cauchy", "chisq", "exp", "f", "gamma", "geom", "lnorm",
#'                      "logis", "norm", "nbinom", "pois", "t", "weibull".
#'                      Default = "norm".
#'      \item dparams : list of parameters to the chosen theoretical distribution
#'                      function. If an empty list is provided (default), the
#'                      distributional parameters are estimated via MLE.
#'                      Default = list().
#'      \item detrend : logical, Should the resulting Q-Q plot be detrended?
#'                      If TRUE, it'll be detrended according to the reference
#'                      Q-Q line. This procedure was described by Thode (2002),
#'                      and it reduces visual bias caused by orthogonal
#'                      distances from Q-Q points to the reference line.
#'                      Default = TRUE.
#'      \item ci_band_type : string, type of the confidence bands to be drawn:
#'              "pointwise" (default), "boot", "ks", and "ts", where
#'              * "pointwise" - simultaneous confidence bands based on the normal distribution;
#'              * "boot" - pointwise confidence bands based on a parametric boostrap;
#'              * "ks" - simultaneous confidence bands based on an inversion of the Kolmogorov-Smirnov test;
#'                       It's not tail sensitive so the bands at the tails are really wide. Not good to test
#'                       if the tails of the empirical distribution follows the given theoretical distribution.
#'              * "ts" - tail-sensitive confidence bands, as proposed by Aldor-Noiman et al. (2013).
#'                       It only works when dist = "norm", and it takes too long to compute for large samples.
#'      \item font_size: overall font size. Default = 14. The font size of the
#'                       axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_qqplot.R
mk_qqplot = function(df) {
        function(varname, dist = "norm", dparams = list(), detrend = TRUE,
                 ci_band_type = "pointwise", font_size = 14) {

                # --- Prep  --- #

                # calc sample size
                tot_n = length(na.omit(df[[varname]]))
                ssize = paste0("(n = ", tot_n, ")")

                # get default plot type
                plot_type = ifelse(dist == "norm", "Q-Q normal plot", "Q-Q plot")

                if (detrend) { # change ylab and plot type
                        # y is observed sample value - expected value under the
                        #       chosen theoretical distribution
                        #       ref: https://github.com/aloy/qqplotr/blob/master/R/stat_qq_point.R
                        ylab = "Deviations (Observed - Expected)"
                        plot_type = paste("Detrended", plot_type)
                }

                # make default subtitle
                subtit = paste(plot_type, ssize)


                # --- Main Plot --- #

                p = ggplot(df, aes_string(sample = varname, fill = "1"))
                p = p + qqplotr::geom_qq_band(distribution = dist,
                                              detrend = detrend,
                                              dparams = dparams,
                                              bandType = ci_band_type,
                                              alpha = 0.4) +
                        qqplotr::stat_qq_line(distribution = dist,
                                              detrend = detrend,
                                              dparams = dparams) +
                        qqplotr::stat_qq_point(distribution = dist,
                                               detrend = detrend,
                                               dparams = dparams,
                                               shape = 16)

                # --- Format Legend --- #

                # remove legend
                p = p + guides(color = FALSE, fill = FALSE)



                # --- Customize Theme --- #

                p + labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                         subtitle = subtit) + theme_cowplot(font_size)


        }
}
