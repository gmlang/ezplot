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
#' @param se Standard error of a parameter estimate. It can be read off from
#' the table of fit summary displayed on the scatterplot.
#' @param n Sample size.
#' @param k Number of predictors. Default = 1 (simple linear regression).
#' @param conf_level A number between 0 and 1. Default = 0.95.
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

cb_gray = "#999999"
color_palette = c("#E64B3599", "#4DBBD599", "#00A08799", "#3C548899",
                  "#F39B7F99", "#8491B499", "#91D1C299", "#DC000099",
                  "#7E614899", "#B09C8599", "#80000099", "#76767699",
                  "#FFA31999", "#8A904599", "#155F8399", "#C1662299",
                  "#8F393199", "#58593F99", "#350E2099",
                  "#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01",
                  "#a6cee3","#fb9a99","#984ea3","#8C591D")


#' @title CDF of the Pareto Distribution.
#'
#' @description
#' Cumulative Distribution Function for the pareto distribution with shape
#' `shape` and location `location`.
#'
#' @param q vector of quantiles.
#' @param shape vector of shapes. Default = 1.
#' @param location vector of locations. Default = 0.
#'
#' @return A vector of probabilities.
#'
#' @export
#' @examples ppareto(seq(1, 5, 1), shape = 3, location = 1)
ppareto = function(q, shape = 1, location = 0)
        ifelse(q > location, 1 - (location / q) ^ shape, 0)
