#' @title Format numbers as percent.
#
#' @description
#' \code{format_as_pct()} takes a numeric argument \code{x} and returns a 
#' character vector containing the original numeric vector in percent format.
#' 
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places to be used.
#' @return a character vector that's just the original vector in percent format.
#' @export
#' @examples
#' a = c(0.84984554, 0.18, 0.9273, 0.128294)
#' format_as_pct(a)
format_as_pct = function(x, digits=4) paste0(round(x, digits) * 100, "%")