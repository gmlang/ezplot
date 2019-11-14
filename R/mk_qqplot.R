#' @title Create a function for making publishable ggplot2 Q-Q plots.
#'
#' @description
#' \code{mk_qqplot} takes a data frame as input and returns a function for
#' making Q-Q plot of any continuous variable from the data frame.
#'
#' @param df A data frame.
#' @return
#' \code{function(varname, dist = "norm", dparams = list(),
#'                ci_band_type = "pointwise", font_size = 14)}
#' \itemize{
#'      \item varname. String, name of a continuous variable. We're interested
#'      in comparing its empirical distribution with a theoretical distribution,
#'      for example, the standard normal distribution.
#'      \item dist. String, theoretical probability distribution function to
#'      compare against. These values are supported: "beta", "cauchy", "chisq",
#'      "exp", "f", "gamma", "geom", "lnorm", "logis", "norm" (default),
#'      "nbinom", "pois", "t", "weibull".
#'      \item dparams. List of parameters to the chosen theoretical distribution
#'      function. If an empty list is provided (default), the distributional
#'      parameters are estimated via MLE. Default = list().
#'      \item ci_band_type. String, type of the confidence bands to be drawn:
#'      "pointwise" (default), "boot", "ks", and "ts", where
#'          * "pointwise" - simultaneous confidence bands based on the normal distribution;
#'          * "boot" - pointwise confidence bands based on a parametric boostrap;
#'          * "ks" - simultaneous confidence bands based on an inversion of the Kolmogorov-Smirnov test;
#'                   It's not tail sensitive so the bands at the tails are really wide. Not good to test
#'                   if the tails of the empirical distribution follows the given theoretical distribution.
#'          * "ts" - tail-sensitive confidence bands, as proposed by Aldor-Noiman et al. (2013).
#'                   It only works when dist = "norm", and it takes too long to compute for large samples.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_qqplot.R
mk_qqplot = function(df) {
        function(varname, dist = "norm", dparams = list(),
                 ci_band_type = "pointwise", font_size = 14) {

                # --- Prep  --- #

                # set x y limits based on the range of the sample values
                # lim = c(floor(min(df[[varname]], na.rm = TRUE)),
                #         ceiling(max(df[[varname]], na.rm = TRUE)))
                lim = pretty(df[[varname]])[c(1, 5)]

                # calc sample size
                tot_n = length(na.omit(df[[varname]]))

                # make default subtitle
                subtit = paste("Q-Q plot", paste0("(n = ", tot_n, ")"))

                # --- Main Plot --- #

                p = ggplot(df, aes_string(sample = varname, fill = "1"))
                p = p + qqplotr::stat_qq_point(distribution = dist,
                                               dparams = dparams,
                                               shape = 16) +
                        qqplotr::stat_qq_line(distribution = dist,
                                              dparams = dparams) +
                        qqplotr::stat_qq_band(distribution = dist,
                                              dparams = dparams,
                                              bandType = ci_band_type,
                                              alpha = 0.4) +
                        # make 1-unit on x-axis equal to 1-unit on y-axis, also
                        # make x-axis and y-axis have the same limits. This will
                        # eliminate visual bias.
                        coord_equal(ylim = lim, xlim = lim)


                # --- Format Legend --- #

                # remove legend
                p = p + guides(color = FALSE, fill = FALSE)


                # --- Customize Theme --- #

                p + labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                         subtitle = subtit) + theme_cowplot(font_size)

        }
}
