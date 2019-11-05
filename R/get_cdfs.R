#' @title Compute Cumulative Distribution Functions (CDFs) based on sample data.
#'
#' @description
#' Given a data frame with at least one continuous variable and one categorical
#' variable, we can use this function to find the empirical CDFs of any
#' continuous variable on the data frame, optionally grouped by a categorical
#' variable. These CDFs can then be called to return probabilities for any given
#' x value.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, groupby = "1")}
#' \itemize{
#'      \item xvar. String. Name of a continuous variable over which the
#'      empirical CDF to be calculated.
#'      \item groupby. String. Name of a categorical variable for subsetting the
#'      data frame. Default = "1", indicating no such variable is supplied.
#' }
#'
#' @export
#' @examples
#' library(ezplot)
#'
#' df = data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)), gp = gl(2, 100))
#' CDF = get_cdfs(df)
#'
#' CDF_x = CDF('x') # returns a single CDF that can be called on a number
#' CDF_x(0.23)
#'
#' CDFs_x_by_gp = CDF('x', 'gp') # returns a list of CDFs, one for each group
#' sapply(CDFs_x_by_gp, function(f) f(0.23))
get_cdfs = function(df) {
        function(xvar, groupby = '1') {
                if (groupby == "1") {
                        ecdf(df[[xvar]])
                } else {
                        lapply(split(df, df[[groupby]]),
                               function(subdf) ecdf(subdf[[xvar]]))
                }
        }
}
