#' @title Use a different scale on either axis of a ggplot2 plot.
#
#' @description
#' \code{scale_axis} takes a ggplot2 object as input and applies to its x-axis
#' or y-axis one of the following scales:
#' \itemize{
#'     \item comma . Show numbers in 000,000 format.
#'     \item dollar. Show $ in front of numbers.
#'     \item pct   . Use percent format.
#'     \item log   . Use log transformation.
#'     \item log1p . Use log(x+1) transformation.
#'     \item log10 . Use log10 transformation.
#'     \item log2  . Use log2 transformation.
#'     \item sqrt  . Use squared root transformation.
#'     \item exp   . Use exponential transformation.
#'     \item date  . Use date format.
#' }
#' If a ggplot object has too few breaks on an axis (to see the max value), and
#' if you don't need to apply any scales to the axis (due to the values are
#' already in a nice range), you can apply scale='default' to that axis, and
#' this will add nticks (a number supplied by you) from the min to the max value
#' on that axis.
#'
#' @param p A ggplot2 object.
#' @param axis A string of value "x" or "y". Default = "y".
#' @param scale A string of value "default", "comma", "dollar", "pct", "log",
#' "log1p", "log10", "log2", "sqrt", "exp", or 'date'.
#' @param nticks Number of ticks on axis. Default = 10. The actual number of
#' ticks shown will be 1 or 2 less or more that the number user supplied depends
#' on the actual limit of the data. This is caused by rounding in computation.
#' @param digits Number of digits after the decimal point when using the 'pct'
#' scale. Default uses best guess. It's only effective when `scale = 'pct'`.
#'
#' @return A ggplot2 object with the new scale applied to the input axis.
#' @export
#' @examples inst/examples/ex-scale_axis.R
scale_axis = function(p, axis = "y", scale = "default", nticks = 10, digits) {
        # Starting ggplot2 v3.1.1, setting `limit` and `breaks` to the range of
        # the axis inside scale_x_continuous() or scale_y_continuous() will
        # throw this warning:
        #       Removed 2 rows containing missing values (geom_bar)
        # although no data are actually removed. This is a bug as documented
        # here: https://github.com/tidyverse/ggplot2/issues/2887
        # To fix, I tried using `coord_cartesian(ylim=)` and
        # `coord_cartesian(xlim=)`, but they produce unsatisfactory figures.
        # My final solutiont was to insert `oob = function(x, limits) x`
        # inside scale_x_continuous() or scale_y_continuous().

        # extract data along x or y axis
        d = layer_data(p)
        if (axis %in% names(d)) {
                vec = d[[axis]]
                min_val = min(vec[is.finite(vec)], na.rm = T)
                max_val = max(vec[is.finite(vec)], na.rm = T)
                if (class(vec) == 'numeric') min_val = min(min_val, 0)
        } else { # when p is a boxplot, 'xmax_final' or 'ymax_final' and
                 # 'xmin_final' or 'ymin_final' are there instead
                min_val = d[[paste0(axis, 'min_final')]]
                max_val = d[[paste0(axis, 'max_final')]]
        }
        if (scale != 'date') axis_breaks = pretty(c(min_val, max_val), nticks)


        # get function that converts numeric to percent format
        if (missing(digits)) {
                to_pct = scales::percent
        } else {
                to_pct = function(v) scales::percent(v, accuracy = 10^(-digits))
        }

        # --- Main --- #

        if (axis == "y") {
                switch(scale,
                       default = p + # coord_cartesian(ylim = range(axis_breaks))+
                               scale_y_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x
                                                  ),
                       comma = p + # coord_cartesian(ylim = range(axis_breaks)) +
                               scale_y_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = scales::comma),
                       dollar = p + # coord_cartesian(ylim = range(axis_breaks)) +
                               scale_y_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = scales::dollar),
                       pct = p + # coord_cartesian(ylim = range(axis_breaks)) +
                               scale_y_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = to_pct),
                       log = p + scale_y_continuous(
                               trans = scales::log_trans(),
                               breaks = scales::trans_breaks(
                                       'log', function(x) exp(x), n=nticks),
                               labels = scales::trans_format(
                                       'log', scales::math_format(e^.x))
                               ),
                       log1p = p + scale_y_continuous(
                               trans = scales::log1p_trans(),
                               breaks = scales::trans_breaks(
                                       'log1p', function(x) exp(x+1), n=nticks),
                               labels = scales::trans_format(
                                       'log1p', scales::math_format(
                                               e^.x, function(x) round(x, 2)))
                               ),
                       log10 = p + scale_y_log10(
                               trans = scales::log10_trans(),
                               breaks = scales::trans_breaks(
                                       'log10', function(x) 10^x, n = nticks),
                               labels = scales::trans_format(
                                       'log10', scales::math_format(10^.x))
                               ),
                       log2 = p + scale_y_continuous(
                               trans = scales::log2_trans(),
                               breaks = scales::trans_breaks(
                                       'log2', function(x) 2^x, n = nticks),
                               labels = scales::trans_format(
                                       'log2', scales::math_format(2^.x))
                               ),
                       sqrt = p + scale_y_continuous(
                               trans = scales::sqrt_trans(),
                               breaks = scales::trans_breaks(
                                       'sqrt', function(x) x^2, n = nticks),
                               labels = scales::trans_format(
                                       'sqrt', scales::math_format(.x^2))
                               ),
                       exp = p + scale_y_continuous(
                               trans = scales::exp_trans(),
                               breaks = scales::trans_breaks(
                                       'exp', function(x) log(x), n = nticks),
                               labels = scales::trans_format(
                                       'exp', scales::math_format(ln(.x)))
                               ),
                       date = p + scale_y_date(
                               breaks = scales::pretty_breaks(nticks),
                               label = scales::label_date_short()
                               )
                )

        } else {

                switch(scale,
                       default = p + # coord_cartesian(xlim = range(axis_breaks))+
                               scale_x_continuous(breaks = axis_breaks
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x
                                                  ),
                       comma = p + # coord_cartesian(xlim = range(axis_breaks)) +
                               scale_x_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = scales::comma),
                       dollar = p + # coord_cartesian(xlim = range(axis_breaks)) +
                               scale_x_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = scales::dollar),
                       pct = p + # coord_cartesian(xlim = range(axis_breaks)) +
                               scale_x_continuous(breaks = axis_breaks,
                                                  # limits = range(axis_breaks),
                                                  # oob = function(x, limits) x,
                                                  labels = to_pct),
                       log = p + scale_x_continuous(
                               trans = scales::log_trans(),
                               breaks = scales::trans_breaks(
                                       'log', function(x) exp(x), n = nticks),
                               labels = scales::trans_format(
                                       'log', scales::math_format(e^.x))
                               ),
                       log1p = p + scale_x_continuous(
                               trans = scales::log1p_trans(),
                               breaks = scales::trans_breaks(
                                       'log1p', function(x) exp(x+1), n=nticks),
                               labels = scales::trans_format(
                                       'log1p', scales::math_format(
                                               e^.x, function(x) round(x, 2)))
                               ),
                       log10 = p + scale_x_log10(
                               trans = scales::log10_trans(),
                               breaks = scales::trans_breaks(
                                       'log10', function(x) 10^x, n = nticks),
                               labels = scales::trans_format(
                                       'log10', scales::math_format(10^.x))
                               ),
                       log2 = p + scale_x_continuous(
                               trans = scales::log2_trans(),
                               breaks = scales::trans_breaks(
                                       'log2', function(x) 2^x, n = nticks),
                               labels = scales::trans_format(
                                       'log2', scales::math_format(2^.x))
                               ),
                       sqrt = p + scale_x_continuous(
                               trans = scales::sqrt_trans(),
                               breaks = scales::trans_breaks(
                                       'sqrt', function(x) x^2, n = nticks),
                               labels = scales::trans_format(
                                       'sqrt', scales::math_format(.x^2))
                               ),
                       exp = p + scale_x_continuous(
                               trans = scales::exp_trans(),
                               breaks = scales::trans_breaks(
                                       'exp', function(x) log(x), n = nticks),
                               labels = scales::trans_format(
                                       'exp', scales::math_format(ln(.x)))
                               ),
                       date = p + scale_x_date(
                               breaks = scales::pretty_breaks(nticks),
                               label = scales::label_date_short()
                               )
                )
        }

}
