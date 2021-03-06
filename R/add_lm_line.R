#' @title Add the best line with (equation, R-squared, p-value) or
#' (fit table, R-squared) to scatterplot.
#'
#' @description
#' \code{add_lm_line} takes a ggplot object (scatterplot) as input and adds
#' the best line to the plot with its equation, the R-squared value, and the
#' p-value of the x term coef. estimate. (Note: because it's simple linear
#' regression, the p-value is the same as the p-value obtained when running
#' correlation test, and the R-squared value is just the correlation squared.)
#' Alternatively, by settting \code{show = "tb"}, user can add to the plot the
#' entire fit table (term, coef. estimate, standard error, tstat and pval) and
#' the R-squared value.
#' If the input scatterplot object has 1+ colored points, it will add 1+ lines
#' with each line corresponding to a color. When this happens, use the default
#' \code{show = "eq"} since printing 1+ fit tables doesn't work.
#'
#' @param p A ggplot object, must be a scatter plot.
#' @param alpha A number between 0 and 1, transparency level of the best line.
#' Smaller value means more transparent. Default = 0.8.
#' @param linew Number, width of the line. Default = 1.
#' @param show String, "eq" or "tb". If "eq" (default), add the equation of
#' the best fit line, the R-squared value, and the p-value of the x term coef.
#' estimate to the plot. If "tb", add the fit table and the R-squared value to
#' the plot.
#' @param eq_tb_xpos Numeric between 0 and 1 or character. x-position of the
#' equation or fit table label. Default = "left".
#' @param eq_tb_ypos Numeric between 0 and 1 or character. y-position of the
#' equation or fit table label. Default = "top".
#' @param pv_r2_xpos Numeric between 0 and 1 or character. x-position of the
#' p-value or R-squared value label. Default = "right".
#' @param pv_r2_ypos Numeric between 0 and 1 or character. y-position of the
#' p-value or R-squared value label. Default = "bottom".
#' @param ... Other arguments for ggpmisc::stat_poly_eq() or ggpmisc::stat_fit_tb().
#' For example, for ggpmisc::stat_poly_eq(), coef.digits sets the number of digits
#' for coef. estimates, rr.digits sets the number of digits for R-squared value.
#' For ggpmisc::stat_fit_tb(), digits sets the digits for all numbers in the fit
#' table.
#'
#' @return A ggplot scatterplot object with the best line added with detailed
#' labels.
#'
#' @export
#' @examples inst/examples/ex-add_lm_line.R
add_lm_line = function(p, alpha = 0.8, linew = 1, show = "eq",
                       eq_tb_xpos = "left", eq_tb_ypos = "top",
                       pv_r2_xpos = "right", pv_r2_ypos = "bottom", ...) {

        p = p + ggplot2::geom_line(stat="smooth", method="lm", formula='y ~ x',
                                   alpha = alpha, size = linew)

        # there're bugs in stat_fit_glance() and stat_fit_tb() that fail to
        # invoke broom::glance() on a lm object. So we explicitly call
        # broom::glance() here as a hack, otherwise, p-val extraction in
        # stat_fit_glance() and table extraction in stat_fit_tb() will fail
        broom::glance()

        if (show == "eq") {
                p = p + ggpmisc::stat_poly_eq(
                        ggplot2::aes(label = paste(..eq.label..,
                                                   ..rr.label..,
                                                   sep = "*\",\"~~")),
                        formula = y ~ x,
                        parse = TRUE, # allows italic to work
                        label.x = eq_tb_xpos,
                        label.y = eq_tb_ypos,
                        ...)
                p = p + ggpmisc::stat_fit_glance(
                        ggplot2::aes(
                                label = paste('~italic(p)~-value',
                                              ifelse(..p.value.. < 0.001, "< 0.001",
                                                     paste('~"="~', round(..p.value.., 3))))
                                ),
                        # method = 'lm',
                        # method.args = list(formula = y ~ x),
                        # glance.args = list(formula = y ~ x),
                        parse = TRUE, # allows italic to work
                        label.x = pv_r2_xpos,
                        label.y = pv_r2_ypos)
        }

        if (show == "tb") {
                p = p + ggpmisc::stat_fit_tb(
                        tb.type = "fit.summary",
                        tb.vars = c("term",
                                    "est" = "estimate",
                                    "se" = "std.error",
                                    "~italic(t)~stat" = "statistic",
                                    "~italic(p)~val" = "p.value"),
                        parse = TRUE, # allows italic to work
                        label.x = eq_tb_xpos,
                        label.y = eq_tb_ypos,
                        ...)
                p = p + ggpmisc::stat_fit_glance(
                        ggplot2::aes(label = sprintf('~italic(R^2)~"="~%.3f',
                                                     ..r.squared..)),
                        method.args = list(formula = y ~ x),
                        parse = TRUE, # allows italic to work
                        label.x = pv_r2_xpos,
                        label.y = pv_r2_ypos)
        }
        p
}




