#' @title Estimate the parameters of the weibull distribution from which the
#' sample data were taken.
#'
#' @description
#' Given a data frame and the name of a continuous variable from the data frame,
#' suppose the -logCCDF of the variable on a log-log scale is linear, then its
#' distribution can be modeled by the weibull distribution, of which the
#' parameters can be estimated by this function.
#'
#' @param df A data frame.
#' @param varname. String, name of a continuous variable with an empirical
#' distribution approximately weibull.
#' @param digits. Integer, the number of digits after decimal point to keep for
#' the estimated parameter values.
#'
#' @return Estimates of the shape (k) and location (lambda) parameters of the
#' weibull distribution.
#'
#' @export
#' @examples
#' library(ezplot)
#' est_params_weibull(births, 'diffs')
#' est_params_weibull(births, 'diffs', digits = 4)
est_params_weibull = function(df, varname, digits = 3) {
        cdf = get_cdfs(df)(varname)
        xs = df[[varname]]
        ys = -log(1 - cdf(xs))
        dat = data.frame(x = log(xs), y = log(ys)) %>% # taking log may introduce -Inf
                filter(is.finite(x), is.finite(y)) # which we need to drop

        fit = lm(y ~ x, data = dat)
        slope = setNames(coef(fit)['x'], NULL)
        intercept = setNames(coef(fit)['(Intercept)'], NULL)

        k = slope
        lambda = exp(-intercept / k)

        params = c(shape = k, location = lambda)
        round(params, digits)
}

