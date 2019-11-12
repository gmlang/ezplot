#' @title Estimate the rate of the exponential distribution from which the
#' sample data were taken.
#'
#' @description
#' Given a data frame and the name of a continuous variable from the data frame,
#' suppose the CCDF of the variable on a log-y scale is linear, then its
#' distribution can be modeled by the exponential distribution, of which the
#' rate can be estimated by this function.
#'
#' @param df A data frame.
#' @param varname. String, name of a continuous variable with an empirical
#' distribution approximately exponential.
#' @param digits. Integer, the number of digits after decimal point to keep for
#' the estimated parameter values.
#'
#' @return An estimate of the rate of a theoretical exponential distribution.
#'
#' @export
#' @examples
#' library(ezplot)
#' est_params_expdist(births, 'diffs')
#' est_params_expdist(births, 'diffs', digits = 4)
est_params_expdist = function(df, varname, digits = 3) {
        cdf = get_cdfs(df)(varname)
        xs = df[[varname]]
        ys = log(1 - cdf(xs))
        df_logCCDF = data.frame(x = xs, y = ys)
        df_logCCDF = df_logCCDF[is.finite(df_logCCDF$y),]

        fit = lm(y ~ x, data = df_logCCDF)
        slope = setNames(coef(fit)['x'], NULL)
        rate = abs(slope)
        # avg = round(1 / rate, 2) # mean time between events
        # std = round(1 / rate, 2) # standard deviation of time between events

        round(rate, digits)
}

