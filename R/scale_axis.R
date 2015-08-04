#' @title Helper function used by \code{scale_axis}. Not for external use.
#' 
#' @description \code{scale_axis_helper} takes in R code strings, combines them
#' and returns an expression to be evaluated by \code{scale_axis}.
#' 
#' @param lstr string containing the left side of ggplot2 code.
#' @param axis string of value "x" or "y".
#' @param rstr string containing the right side of ggplot2 code.
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
#' \code{scale_axis} takes a ggplot2 object as input and applies to its x-axis or 
#' y-axis one of the following scales: 
#' \itemize{
#'      \item comma: display big numbers in 000,000 format.
#'      \item log  : use log scale.
#'      \item log10: use log10 scale.
#'      \item pct  : use % format, the values must be between 0 and 1.
#' }
#' It uses \code{scale_axis_helper} as a helper. 
#' 
#' @seealso \code{\link{scale_axis_helper}}.
#' 
#' @param p A ggplot2 object.
#' @param axis A string of value "x" or "y". Default = "y". 
#' @param scale A string of value "comma", "log", "log10", or "pct". It specifies which scale to use. Default = NULL, which uses the original scale.
#' @param pct_max A number no bigger than 1. It specifies the max value when using percent format. Default = 1.
#' @param pct_jump A number between 0 and 1. It specifies the interval width between ticks when using percent format. Default = 0.2.
#' 
#' @return A ggplot2 object with the new scale applied to the input axis.      
#' @export
#' @examples
#' # make some fake data
#' library(ezplot)
#' plt = mk_scatterplot(films)
#' 
#' p = plt("boxoffice", "budget", pt_alpha=0.5, pt_size=1)
#' p = scale_axis(p, "y", scale = "log10")
#' print(p)
#' 
#' p = scale_axis(p, "x", scale = "log2")
#' print(p)
#' 
#' # make some data
#' df = read.table(header=TRUE, text='
#' student grade
#' Joe 90
#' Mary 75
#' Alex 50')
#' df$pct = df$grade / sum(df$grade)
#' 
#' barplt = mk_barplot(df)
#' p = barplt("student", "pct", fillby="student", legend=F)
#' scale_axis(p, "y", scale="pct", pct_jump=0.3)
scale_axis = function(p, axis="y", scale=NULL, pct_max=1, pct_jump=0.2) {
        x = deparse(substitute(p))
        l = paste(x, "=", x, "+ ggplot2::scale_")
        r_comma = "_continuous(labels = scales::comma)"
        r_log = "_continuous(trans = scales::log_trans(),
                breaks = scales::trans_breaks('log', function(x) exp(x)),
                labels = scales::trans_format('log', scales::math_format('e'^.x)))"
        r_log2 = "_continuous(trans = scales::log2_trans(),
                breaks = scales::trans_breaks('log2', function(x) 2^x),
                labels = scales::trans_format('log2', scales::math_format('2'^.x)))"
        r_log10 = "_log10(breaks = scales::trans_breaks('log10', function(x) 10^x),
                labels = scales::trans_format('log10', scales::math_format(10^.x)))"
        r_pct = paste0("_continuous(labels = scales::percent, limits = c(0, ",
                       pct_max, "), breaks = seq(0,", pct_max, ",", pct_jump, "))")        
        
        pexpr = NULL # need this for it to work when using knitr       
        pexpr = switch(scale,
                       comma = scale_axis_helper(l, axis, r_comma),
                       log = scale_axis_helper(l, axis, r_log),
                       log2 = scale_axis_helper(l, axis, r_log2),
                       log10 = scale_axis_helper(l, axis, r_log10),
                       pct = scale_axis_helper(l, axis, r_pct))
        p = eval(pexpr)
        p
}
