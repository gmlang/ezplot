#' @title Estimate the shape and location parameters of the pareto distribution
#' from which the sample data were taken.
#'
#' @description
#' Given a data frame and the name of a continuous variable from the data frame,
#' suppose the CCDF of the variable on a log10-log10 scale is linear, then its
#' distribution can be modeled by the pareto distribution, of which the shape
#' and location parameters can be estimated by this function.
#'
#' @param df A data frame.
#' @param varname. String, name of a continuous variable with an empirical
#' distribution approximately pareto.
#' @param digits. Integer, the number of digits after decimal point to keep for
#' the estimated parameter values.
#'
#' @return Estimates of the shape and location parameters of a theoretical
#' pareto distribution.
#'
#' @export
#' @examples
#' library(ezplot)
#' est_params_pareto(pops, 'pop', digits = 2)
est_params_pareto = function(df, varname, digits = 3) {
        cdf = get_cdfs(df)(varname)
        xs = df[[varname]]
        ys = 1 - cdf(xs)
        dat = data.frame(x = log10(xs), y = log10(ys)) %>% # taking log10 may introduce -Inf
                filter(is.finite(x), is.finite(y)) # which we need to drop

        fit = lm(y ~ x, data = dat)
        slope = setNames(coef(fit)['x'], NULL)
        intercept = setNames(coef(fit)['(Intercept)'], NULL)

        alpha = -1 * slope
        xm = 10^(intercept / alpha)

        params = c(shape = alpha, location = xm)
        round(params, digits)
}

