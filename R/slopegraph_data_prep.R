#' @title Calculate slope graph positions based on Edward Tufte's layout.
#'
#' @param df A data frame with named x, y, and group columns.
#' @param xvar String, name of the x-axis column.
#' @param yvar String, name of the y-axis column.
#' @param min_space Number, fraction of total data range to use as a minimum gap 
#'                  between the lines. Default = 0.05.
#' @param line_gap_scale Number, scale factor when calculating ypos. Default = 1.                         
#' @return A data frame with an additional ypos column.
#' 
#' @seealso \code{\link{slopegraph_data_prep}}
tufte_sort = function(df, xvar, yvar, min_space=0.05, line_gap_scale = 1) {
        # change to wide format and order by first numeric column (2nd col)
        tmp = tidyr::spread_(df, xvar, yvar)
        tmp = tmp[order(tmp[, 2]), ]
        min_space = min_space * diff(range(tmp[, -1]))
        yshift = numeric(nrow(tmp))
        
        # add gaps between lines according to tufte style
        for (i in 2:nrow(tmp)) {
                # shift subsequent row up by equal space so gap between
                # two entries is >= minimum
                mat = as.matrix(tmp[(i-1):i, -1])
                dmin = min(diff(mat))
                yshift[i] = ifelse(dmin < min_space, min_space - dmin, 0)
        }
        
        # change back to long format
        tmp = suppressMessages(tidyr::gather_(tmp, key=xvar, value=yvar))
        
        # append yshift column and use it to calculate ypos
        tmp = cbind(tmp, yshift = cumsum(yshift))
        tmp = within(tmp, {ypos = tmp[[yvar]] + line_gap_scale * yshift})
        
        # return tmp
        tmp
}


#' @title Build a slopegraph data set.
#' 
#' @description
#' Modify a data frame so that it can be used to plot tufte slopegraph. A tufte
#' slopegraph has the following characteristics: values in the first x-column 
#' are sorted based on their numeric value. Subsequent group lines are then 
#' shifted to ensure that the lines for two adjacent groups never cross. 
#' Vertical positions in subsequent columns are only meaningful relative to the 
#' first entry in that group. 
#' 
#' @details It uses the function \code{\link{tufte_sort}} internally, which does
#' the heavy lifting of the above mentioned tufte slopegraph format.
#' 
#' @param df A data frame.
#' @param xvar String, name of the x-axis column. 
#' @param yvar String, name of the y-axis column.
#' @param gpvar String, name of the group column. 
#' @param min_space Number, fraction of total data range to use as a minimum gap 
#'                  between the lines. Default = 0.05.
#' 
#' @return A data frame with an additional ypos column.
#' @export
#' 
#' @seealso \code{\link{tufte_sort}}
slopegraph_data_prep = function(df, xvar, yvar, gpvar, min_space=0.05, ...) {
        # expand grid to ensure every combination has a defined value
        tmp = expand.grid(unique(df[[xvar]]), unique(df[[gpvar]]), 
                          stringsAsFactors=F)
        names(tmp) = c(xvar, gpvar)
        df = merge(df, tmp, all.y=TRUE)
        df[[yvar]][is.na(df[[yvar]])] = 0 # replace NA by 0
        
        # sort df according to tufte display format and return it
        tufte_sort(df, xvar, yvar, min_space, ...)
}



