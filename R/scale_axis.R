#' @title Helper function used by \code{scale_axis}. Not for external use.
#'
#' @description \code{scale_axis_helper} takes in R code strings, combines them
#' and returns an expression to be evaluated by \code{scale_axis}.
#'
#' @param lstr string, ggplot2 code that comes before "x" or "y".
#' @param axis string, "x" or "y".
#' @param rstr string, ggplot2 code that comes after "x" or "y".
#'
#' @return a R expression to be evaluated by \code{scale_axis}.
#'
#' @seealso \code{\link{scale_axis}}.
scale_axis_helper = function(lstr, axis, rstr) {
        out_str = paste0(lstr, axis, rstr)
        parse(text = out_str)
}

#' @title Use a different scale on either axis of a ggplot2 plot.
#
#' @description
#' \code{scale_axis} takes a ggplot2 object as input and applies to its x-axis
#' or y-axis one of the following scales:
#' \itemize{
#'     \item comma  : show numbers in 000,000 format.
#'     \item dollar : show $ in front of numbers.
#'     \item pct    : use % format.
#'     \item log    : use log transformation.
#'     \item log1p  : use log(x+1) transformation.
#'     \item log10  : use log10 transformation.
#'     \item log2   : use log2 transformation.
#'     \item sqrt   : use squared root transformation.
#'     \item exp    : use exponential transformation.
#' }
#' It uses \code{scale_axis_helper} as a helper.
#'
#' @seealso \code{\link{scale_axis_helper}}.
#'
#' @param p A ggplot2 object.
#' @param axis A string of value "x" or "y". Default = "y".
#' @param scale A string of value "comma", "dollar", "pct", "log", "log1p",
#'        "log10", "log2", "sqrt", or "exp".
#'         It specifies which scale to use. Default = "comma".
#' @param pct_min A number. The min value when using percent scale.
#'         When NULL (default), it'll use the min value of y.
#' @param pct_max A number. The max value when using percent scale.
#'         When NULL (default), it'll use the max value of y.
#' @param pct_jump A number. The equal distance between ticks when using
#'         percent scale. When NULL (default), it takes the value of
#'         (pct_max - pct_min) / 10, and this will break the y-axis
#'         into 10 pieces by placing 11 ticks on it.
#'
#' @return A ggplot2 object with the new scale applied to the input axis.
#' @export
#' @examples
#' library(ezplot)
#'
#' #' # make some data
#' df = read.table(header=TRUE, text='
#' IQ score
#' 125 0.86
#' 110 0.72
#' 108 0.64')
#'
#' plt = mk_scatterplot(df)
#' p = plt("IQ", "score", pt_size = 2)
#' print(p)
#' scale_axis(p, scale = "pct")
#' scale_axis(p, scale = "pct", pct_min = 0)
#' scale_axis(p, scale = "pct", pct_max = 1.2)
#' scale_axis(p, scale = "pct", pct_jump = 0.05)
#' scale_axis(p, scale = "pct", pct_min = 0, pct_max = 1)
#' scale_axis(p, scale = "pct", pct_max = 1, pct_min = 0, pct_jump = 0.2)
#'
#' plt = mk_scatterplot(films)
#' p = plt("boxoffice", "budget", alpha=0.5, pt_size=1)
#' print(p)
#' scale_axis(p)
#' scale_axis(p, scale = "dollar")
#' p = scale_axis(p, scale = "log10")
#' print(p)
#' p = scale_axis(p, "x", scale = "log2")
#' print(p)
#'
#' library(dplyr)
#' plt = mk_scatterplot(films %>% filter(bo_bt_ratio < 1))
#' p = plt("votes", "bo_bt_ratio", alpha=0.5, pt_size=1)
#' print(p)
#' scale_axis(p, scale = "sqrt")
#' scale_axis(p, scale = "exp")
#' scale_axis(p, "x", scale = "log")
#' scale_axis(p, "x", scale = "log2")
#' scale_axis(p, "x", scale = "log10")
#' scale_axis(p, "x", scale = "log1p")
scale_axis = function(p, axis = "y", scale = "comma",
                      pct_min = NULL, pct_max = NULL, pct_jump = NULL) {
        x = deparse(substitute(p))
        l = paste(x, "=", x, "+ ggplot2::scale_")
        r_comma = "_continuous(labels = scales::comma)"
        r_dollar = "_continuous(labels = scales::dollar)"

        if (scale == "pct") {
                var = p$labels[[axis]]
                val_min = min(c(0, p$data[[var]]), na.rm = T)
                val_max = max(p$data[[var]], na.rm = T)
                pct_min = ifelse(is.null(pct_min), val_min, pct_min)
                pct_max = ifelse(is.null(pct_max), val_max, pct_max)
                pct_jump = ifelse(is.null(pct_jump), (pct_max - pct_min) / 10,
                                  pct_jump)
                r_pct = paste0("_continuous(labels = scales::percent, limits = c(",
                               pct_min, ",", pct_max+pct_jump, "), breaks = seq(",
                               pct_min, ",", pct_max+pct_jump, ",", pct_jump, "))")
        }

        r_log = "_continuous(trans = scales::log_trans(),
                breaks = scales::trans_breaks('log', function(x) exp(x), n=10),
                labels = scales::trans_format('log', scales::math_format(e^.x)))"
        r_log1p = "_continuous(trans = scales::log1p_trans(),
                breaks = scales::trans_breaks('log1p', function(x) exp(x+1), n=10),
                labels = scales::trans_format('log1p', scales::math_format(e^.x, function(x) round(x, 2))))"
        r_log10 = "_log10(breaks = scales::trans_breaks('log10', function(x) 10^x, n=10),
                labels = scales::trans_format('log10', scales::math_format(10^.x)))"
        r_log2 = "_continuous(trans = scales::log2_trans(),
                breaks = scales::trans_breaks('log2', function(x) 2^x, n=10),
                labels = scales::trans_format('log2', scales::math_format(2^.x)))"

        r_sqrt = "_continuous(trans = scales::sqrt_trans(),
                breaks = scales::trans_breaks('sqrt', function(x) x^2, n=10),
                labels = scales::trans_format('sqrt', scales::math_format(.x^2)))"
        r_exp = "_continuous(trans = scales::exp_trans(),
                breaks = scales::trans_breaks('exp', function(x) log(x), n=10),
                labels = scales::trans_format('exp', scales::math_format(ln(.x))))"

        pexpr = NULL # need this for it to work when using knitr
        pexpr = switch(scale,
                       comma = scale_axis_helper(l, axis, r_comma),
                       dollar = scale_axis_helper(l, axis, r_dollar),
                       pct = scale_axis_helper(l, axis, r_pct),
                       log = scale_axis_helper(l, axis, r_log),
                       log1p = scale_axis_helper(l, axis, r_log1p),
                       log10 = scale_axis_helper(l, axis, r_log10),
                       log2 = scale_axis_helper(l, axis, r_log2),
                       sqrt = scale_axis_helper(l, axis, r_sqrt),
                       exp = scale_axis_helper(l, axis, r_exp)
                       )
        p = eval(pexpr)
        p
}
