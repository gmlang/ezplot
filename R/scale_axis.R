#' @title Helper function used by \code{scale_axis()}. Not for external use.
#' 
#' @description \code{scale_axis_helper()} takes in R code strings, combines them
#' and returns an expression to be evaluated by \code{scale_axis()}.
#' 
#' @param lstr string containing the left side of ggplot2 code.
#' @param axis string of value "x" or "y".
#' @param rstr string containing the right side of ggplot2 code.
#' 
#' @return a R expression to be evaluated by \code{scale_axis()}.  
#' 
#' @seealso \code{\link{scale_axis}}.
scale_axis_helper = function(lstr, axis, rstr) {
        out_str = eval(quote(paste0(lstr, axis, rstr)))
        parse(text = out_str)
}

#' @title Add different scales to either axis of a ggplot2 plot.
#
#' @description
#' \code{scale_axis()} takes a ggplot2 object as input and adds to its x-axis or 
#' y-axis one of the following four scales: 
#' \itemize{
#'      \item comma: display big numbers in 000,000 format instead of scientific notation.
#'      \item log  : use log scale.
#'      \item log10: use log10 scale.
#'      \item pct  : use % format, the values must already be decimal.
#' }
#' 
#' @param p ggplot2 object.
#' @param axis string of value "x" or "y". Default = "y". 
#' @param use_comma logical, whether to use comma scale or not. Default = FALSE.
#' @param use_log logical, whether to use log scale or not. Default = FALSE.
#' @param use_log10 logical, whether to use log10 scale or not. Default = FALSE.
#' @param use_pct logical, whether to use percent format or not. Default = FALSE.
#' @param pct_jump numeric, intervals at which ticks are displayed when using percent format. Default = 0.2.
#' 
#' @return a ggplot2 object with the new scale added to the selected axis.      
#' @export
#' @examples
#' # make some fake data
#' library(ezplot)
#' plt = mk_scatterplot(films)
#' 
#' p = plt("boxoffice", "budget", pt_alpha=0.5, pt_size=1)
#' p = scale_axis(p, "y", use_log10=T)
#' print(p)
#' 
#' p = scale_axis(p, "x", use_log10=T)
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
#' scale_axis(p, "y", use_pct=T, pct_jump=0.3)
scale_axis = function(p, axis="y", use_comma=F, use_log=F, use_log10=F, 
                      use_pct=F, pct_jump=0.2) {
        # axis = "x", "y"
        x = deparse(substitute(p))
        l = paste(x, "=", x, "+ ggplot2::scale_")
        r_comma = "_continuous(labels = scales::comma)"
        r_log = "_continuous(trans = scales::log_trans(),
                breaks = scales::trans_breaks('log', function(x) exp(x)),
                labels = scales::trans_format('log', scales::math_format('e'^.x)))"
        r_log10 = "_log10(breaks = scales::trans_breaks('log10', function(x) 10^x),
                labels = scales::trans_format('log10', scales::math_format(10^.x)))"
        r_pct = paste0("_continuous(labels = scales::percent, limits = c(0,1),
                       breaks = seq(0, 1, ", pct_jump, "))")        
                
        if (use_comma)
                pexpr = scale_axis_helper(l, axis, r_comma)
        if (use_log)
                pexpr = scale_axis_helper(l, axis, r_log)
        if (use_log10)
                pexpr = scale_axis_helper(l, axis, r_log10)
        if (use_pct)
                pexpr = scale_axis_helper(l, axis, r_pct)
        p = eval(pexpr)
        p
}




