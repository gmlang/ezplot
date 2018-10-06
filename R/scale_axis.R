#' @title Use a different scale on either axis of a ggplot2 plot.
#
#' @description
#' \code{scale_axis} takes a ggplot2 object as input and applies to its x-axis
#' or y-axis one of the following scales:
#' \itemize{
#'     \item comma  : show numbers in 000,000 format.
#'     \item dollar : show $ in front of numbers.
#'     \item pct    : use percent format.
#'     \item log    : use log transformation.
#'     \item log1p  : use log(x+1) transformation.
#'     \item log10  : use log10 transformation.
#'     \item log2   : use log2 transformation.
#'     \item sqrt   : use squared root transformation.
#'     \item exp    : use exponential transformation.
#' }
#'
#' @param p A ggplot2 object.
#' @param axis A string of value "x" or "y". Default = "y".
#' @param scale A string of value "comma", "dollar", "pct", "log", "log1p",
#'        "log10", "log2", "sqrt", or "exp".
#'         It specifies which scale to use. Default = "comma".
#'
#' @return A ggplot2 object with the new scale applied to the input axis.
#' @export
#' @examples inst/examples/ex-scale_axis.R
scale_axis = function(p, axis = "y", scale = "comma") {

        var = as.character(p$mapping[[axis]])[[2]]
        axis_breaks = pretty(c(0, p$data[[var]]), 10)

        if (axis == "y") {
                switch(scale,
                       comma = p + scale_y_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::comma),
                       dollar = p + scale_y_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::dollar),
                       pct = p + scale_y_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::percent
                               ),
                       log = p + scale_y_continuous(
                               trans = scales::log_trans(),
                               breaks = scales::trans_breaks(
                                       'log', function(x) exp(x), n = 8),
                               labels = scales::trans_format(
                                       'log', scales::math_format(e^.x))
                               ),
                       log1p = p + scale_y_continuous(
                               trans = scales::log1p_trans(),
                               breaks = scales::trans_breaks(
                                       'log1p', function(x) exp(x+1), n = 8),
                               labels = scales::trans_format(
                                       'log1p', scales::math_format(
                                               e^.x, function(x) round(x, 2)))
                               ),
                       log10 = p + scale_y_log10(
                               breaks = scales::trans_breaks(
                                       'log10', function(x) 10^x, n = 8),
                               labels = scales::trans_format(
                                       'log10', scales::math_format(10^.x))
                               ),
                       log2 = p + scale_y_continuous(
                               trans = scales::log2_trans(),
                               breaks = scales::trans_breaks(
                                       'log2', function(x) 2^x, n = 8),
                               labels = scales::trans_format(
                                       'log2', scales::math_format(2^.x))
                               ),
                       sqrt = p + scale_y_continuous(
                               trans = scales::sqrt_trans(),
                               breaks = scales::trans_breaks(
                                       'sqrt', function(x) x^2, n = 8),
                               labels = scales::trans_format(
                                       'sqrt', scales::math_format(.x^2))
                               ),
                       exp = p + scale_y_continuous(
                               trans = scales::exp_trans(),
                               breaks = scales::trans_breaks(
                                       'exp', function(x) log(x), n = 8),
                               labels = scales::trans_format(
                                       'exp', scales::math_format(ln(.x)))
                               )
                )

        } else {

                switch(scale,
                       comma = p + scale_x_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::comma),
                       dollar = p + scale_x_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::dollar),
                       pct = p + scale_x_continuous(
                               limits = range(axis_breaks),
                               breaks = axis_breaks,
                               labels = scales::percent
                               ),
                       log = p + scale_x_continuous(
                               trans = scales::log_trans(),
                               breaks = scales::trans_breaks(
                                       'log', function(x) exp(x), n = 8),
                               labels = scales::trans_format(
                                       'log', scales::math_format(e^.x))
                               ),
                       log1p = p + scale_x_continuous(
                               trans = scales::log1p_trans(),
                               breaks = scales::trans_breaks(
                                       'log1p', function(x) exp(x+1), n = 8),
                               labels = scales::trans_format(
                                       'log1p', scales::math_format(
                                               e^.x, function(x) round(x, 2)))
                               ),
                       log10 = p + scale_x_log10(
                               breaks = scales::trans_breaks(
                                       'log10', function(x) 10^x, n = 8),
                               labels = scales::trans_format(
                                       'log10', scales::math_format(10^.x))
                               ),
                       log2 = p + scale_x_continuous(
                               trans = scales::log2_trans(),
                               breaks = scales::trans_breaks(
                                       'log2', function(x) 2^x, n = 8),
                               labels = scales::trans_format(
                                       'log2', scales::math_format(2^.x))
                               ),
                       sqrt = p + scale_x_continuous(
                               trans = scales::sqrt_trans(),
                               breaks = scales::trans_breaks(
                                       'sqrt', function(x) x^2, n = 8),
                               labels = scales::trans_format(
                                       'sqrt', scales::math_format(.x^2))
                               ),
                       exp = p + scale_x_continuous(
                               trans = scales::exp_trans(),
                               breaks = scales::trans_breaks(
                                       'exp', function(x) log(x), n = 8),
                               labels = scales::trans_format(
                                       'exp', scales::math_format(ln(.x)))
                               )
                )
        }

}
