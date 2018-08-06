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
#' @param df A data frame.
#' @return
#' \code{function(p, alpha = 0.8, linew = 1, eq_xpos = "left", eq_ypos = "top",
#'                pval_xpos = "right", pval_ypos = "bottom", ...)}
#' \itemize{
#'      \item p     : a ggplot object, must be a scatter plot.
#'      \item alpha : a number between 0 and 1, transparency level of the
#'                    best line. Smaller value means more transparent.
#'                    Default = 0.8.
#'      \item linew : number, width of the line. Default = 1.
#'      \item eq_xpos: numeric between 0 and 1 or character. x-position
#'                     of the equation label. Default = "left".
#'      \item eq_ypos: numeric between 0 and 1 or character. y-position
#'                     of the equation label. Default = "top".
#'      \item pval_xpos: numeric between 0 and 1 or character. x-position
#'                       of the p-value label. Default = "right".
#'      \item pval_ypos: numeric between 0 and 1 or character. y-position
#'                       of the p-value label. Default = "bottom".
#'      \item ...   : other arguments for ggpmisc::stat_poly_eq(). For example,
#'                    coef.digits sets the number of param coef digits,
#'                    rr.digits sets the number of R2 digits,
#'                    label.x.npc and label.y.npc set the position of the
#'                    line equation.
#' }
#'
#' @export
#' @examples
#' library(ezplot)
#' plt = mk_scatterplot(films)
#'
#' p = plt("budget", "boxoffice", xlab = "Budget", ylab = "Boxoffice")
#' add_lm_line(p)
#' add_lm_line(p, coef.digits = 8, rr.digits = 3, eq_xpos = "right")
#' add_lm_line(p, eq_xpos = "right", pval_ypos = 0.8)
#'
#' p = scale_axis(p, scale = "log10")
#' p = scale_axis(p, "x", scale = "log10")
#' add_lm_line(p, coef.digits = 3, rr.digits = 3)
#' add_lm_line(p, coef.digits = 3, rr.digits = 3,
#'             pval_xpos = "left", pval_ypos = "top", vjust = 0)
#'
#' p = plt("budget", "boxoffice", fillby = "made_money")
#' add_lm_line(p, coef.digits = 8, rr.digits = 3, eq_xpos = "right", pval_ypos = 0.8)
#' p = scale_axis(p, scale = "log")
#' p = scale_axis(p, "x", scale = "log")
#' add_lm_line(p, coef.digits = 3, rr.digits = 3)
#'
#' p = plt("rating", "bo_bt_ratio", fillby = "made_money",
#'         ylab = "Boxoffice / Budget Ratio")
#' add_lm_line(p, coef.digits = 3, rr.digits = 3)
#' p = scale_axis(p, scale = "log10")
#' add_lm_line(p, coef.digits = 3, rr.digits = 3)
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
