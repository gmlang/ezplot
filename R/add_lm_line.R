#' @title Add the best line with equation, R-squared and p-value to scatterplot.
#'
#' @description
#' \code{add_lm_line} takes a ggplot object (scatterplot) as input and adds
#' the best line to the plot with equation, R-squared and p-value of the x term
#' shown as text labels. (Note: because it's simple linear regression, the
#' p-value is the same as the p-value obtained when running correlation test,
#' and the R-squared value is just the correlation squared.)
#' If the input scatterplot object has 1+ colored points, it will add 1+ lines
#' with each line corresponding to a color.
#'
#' @param p : a ggplot object, must be a scatter plot.
#' @param alpha : a number between 0 and 1, transparency level of the best line.
#'                Smaller value means more transparent. Default = 0.8.
#' @param linew : number, width of the line. Default = 1.
#' @param eq_xpos: numeric between 0 and 1 or character. x-position of the
#'                 equation label. Default = "left".
#' @param eq_ypos: numeric between 0 and 1 or character. y-position of the
#'                 equation label. Default = "top".
#' @param pval_xpos: numeric between 0 and 1 or character. x-position of the
#'                   p-value label. Default = "right".
#' @param pval_ypos: numeric between 0 and 1 or character. y-position of the
#'                   p-value label. Default = "bottom".
#' @param ...: other arguments for ggpmisc::stat_poly_eq(). For example,
#'             coef.digits sets the number of param coef digits,
#'             rr.digits sets the number of R2 digits,
#'             label.x.npc and label.y.npc set position of the line equation.
#'
#' @return a ggoplot object with the best line with equation, R-squared and
#'         p-value added.
#'
#' @export
#' @examples inst/examples/ex-add_lm_line.R
add_lm_line = function(p, alpha = 0.8, linew = 1,
                       eq_xpos = "left", eq_ypos = "top",
                       pval_xpos = "right", pval_ypos = "bottom", ...)
        p + ggplot2::geom_line(stat = "smooth", method = "lm",
                               alpha = alpha, size = linew) +
                ggpmisc::stat_poly_eq(
                        ggplot2::aes(label = paste(..eq.label.., ..rr.label..,
                                                   sep = "*\",\"~~")),
                        formula = y ~ x,
                        parse = TRUE, # allows italic to work
                        label.x.npc = eq_xpos, label.y.npc = eq_ypos,
                        ...) +
                ggpmisc::stat_fit_glance(
                        ggplot2::aes(
                                label = paste('~italic(p)~-value',
                                              ifelse(..p.value.. < 0.001,
                                                     "< 0.001",
                                                     paste('~"="~',
                                                           round(..p.value.., 3)
                                                           )
                                                     )
                                              )

                                ),
                        parse = T, # allows italic to work
                        geom = "text",
                        method = "lm",
                        label.x.npc = pval_xpos, label.y.npc = pval_ypos
                        )
