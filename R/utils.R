#' @title Calculate t-based Margin of Error of linear regression parameter
#' estimate.
#'
#' @description
#' When using \code{add_lm_line(p, show = "tb", ...)} to add to the scatter plot
#' \code{p} the best fitting line and the table of fit summary (term, estimate,
#' standard error, tstat and pval), we can take some of the info displayed on
#' the plot and feed them into \code{moe_lm_param()} to calculate the
#' margin of error (moe) of the slope estimate. The slope est +/- moe gives
#' the confidence interval. \code{moe_lm_param()} can also be used to find the
#' margin of error of any parameter estimate of a multiple linear regression.
#'
#' @param se. Standard error of a parameter estimate. It can be
#' read off from the table of fit summary displayed on the scatterplot.
#' @param n. Sample size.
#' @param k. Number of predictors. Default = 1 (simple linear regression).
#' @param conf_level. A number between 0 and 1. Default = 0.95.
#'
#' @return The margin of error of the a linear regression parameter estimate.
#'
#' @export
#' @examples inst/examples/ex-moe_lm_param.R
moe_lm_param = function(se, n, k = 1, conf_level = .95) {
        dof = n - k - 1
        sig_level = 1 - conf_level
        qt(1 - sig_level/2, dof) * se
}
