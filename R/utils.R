#' @title Format numbers as percent.
#
#' @description
#' \code{format_as_pct} takes a numeric argument and returns a 
#' character vector of the original numeric vector in percent format.
#' 
#' @param x A numeric vector.
#' @param digits An integer indicating the number of decimal places to be used.
#' 
#' @return A character vector that's just x in percent format.
#' 
#' @export
#' 
#' @examples
#' a = c(0.84984554, 0.18, 0.9273, 0.128294)
#' format_as_pct(a)
format_as_pct = function(x, digits=4) paste0(round(x, digits) * 100, "%")


#' @title Change missing value code to NA.
#
#' @description
#' \code{missing_fixer} takes a number or a string that represents missing value
#' and returns a function that can be used to replace the number or string 
#' occurred in a vector with NA.
#' 
#' @param na_value A number or a string.
#' 
#' @return 
#' \code{function(x)}
#' \itemize{
#'      \item x     :  a numeric or character vector.
#' }
#' 
#' @export
#' 
#' @examples
#' fix_missing_99 = missing_fixer(-99)
#' fix_missing_999 = missing_fixer(-999)
#' fix_missing_char = missing_fixer("missing")
#' 
#' fix_missing_99(c(-99, -999))
#' fix_missing_999(c(-99, -999))
#' fix_missing_char(c("missing", "male", "female"))
missing_fixer = function(na_value) {
        function(x) {
                x[x == na_value] = NA
                x
        }
}

#' @title Check if a number or a numberic vector is wholenumber or not.
#' 
#' @param x A number or a numeric vector.
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' 
#' @examples
#' is_wholenum(-99)
#' is_wholenum(1)
#' is_wholenum(0.23)
is_wholenum = function(x, tol=.Machine$double.eps^0.5) abs(x - round(x)) < tol

