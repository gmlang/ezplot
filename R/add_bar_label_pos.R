#' @title Create a function that can be used to add two column of label 
#' positions to a data frame.
#
#' @description
#' \code{add_bar_label_pos} can be used to prep data before making bar charts. 
#' It takes a data frame as input and returns a function that can be used to 
#' append columns to the same data frame. Specifically, the output function can 
#' be used to add two extra columns to the original data frame, namely, pos_top 
#' and pos_mid, where pos_top specify the positions when labeling the bars at 
#' the top, and pos_mid specify the positions when labeling the bars at the
#' middle.
#' 
#' @param df a data frame containing variables to be visualized.
#' 
#' @return
#' \code{function(xvar, yvar, vpos=0.2)}
#' \itemize{
#'      \item xvar     :  string, the x variable, often categorical.
#'      \item yvar     :  string, the y variable, often Frequency/Count or Percent.
#'      \item vpos     :  numeric, the extra vertical position beyond the top of each bar. Default is 0.2.
#' }
#' 
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
#' 
#' data2 = f("cat", "freq", vpos=1)
#' data
add_bar_label_pos = function(dat) {
        function(xvar, yvar, vpos=0.2) {
                varname1 = paste(yvar, "pos_top", sep="_")
                varname2 = paste(yvar, "pos_mid", sep="_")

                lst = split(dat, dat[[xvar]])
                lst_out = lapply(lst, function(elt) {
                        top_pos = cumsum(elt[[yvar]])
                        elt[[varname1]] = top_pos + vpos
                        elt[[varname2]] = top_pos - 0.5 * elt[[yvar]] 
                        elt})
                out = do.call("rbind", lst_out)
                row.names(out) = NULL
                out
        }
}
