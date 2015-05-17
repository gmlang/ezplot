#' @title Create a function that adds a column of label positions to a data frame.
#
#' @description
#' \code{add_bar_label_pos()} is used to prepare data before making bar charts. 
#' It takes a data frame as input and returns the same data frame with one extra
#' column of the mid points of the y variable (often Frequency or Percent that is
#' displayed on the y-axis in a bar chart) grouped by the x variable (a 
#' categorical variable displayed on the x-axis in a bar chart).
#' 
#' @param df data frame containing variables to be visualized.
#' 
#' @return a data frame with one extra column indicating bar label positions.
#' @seealso \code{\link{mk_barplot}}.
#' @export
#' @examples
#' # make some fake data
#' year = c(rep(c("2006-07", "2007-08", "2008-09", "2009-10"), each = 4))
#' cat  = c(rep(c("A", "B", "C", "D"), times = 4))
#' freq = c(168, 259, 226, 340, 216, 431, 319, 368, 423, 645, 234, 685, 166, 467, 274, 251)
#' data = data.frame(year, cat, freq)
#' data
#' 
#' f = add_bar_label_pos(data)
#' data = f("year", "freq")
#' data
add_bar_label_pos = function(dat) {
        function(xvar, yvar) {
                lst = split(dat, dat[[xvar]])
                lst_out = lapply(lst, function(elt) {
                        elt$pos = cumsum(elt[[yvar]]) - 0.5 * elt[[yvar]] 
                        elt})
                dat = do.call("rbind", lst_out)
                dat                        
        }
}

