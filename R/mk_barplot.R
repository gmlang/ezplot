#' @title Create a function that draws ggplot2 barplots
#
#' @description
#' \code{mk_barplot()} takes a data frame as input and returns a function that 
#' can be used to make barplots on variables in the data frame.
#' 
#' @param df data frame containing variables to be visualized.
#' 
#' @return
#' \code{function(xvar, yvar, fillby, xlab="", ylab="", main="", legend=T)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item fillby   :  string, the variable used for coloring the bars.
#'      \item xlab     :  string, the x-axis label.
#'      \item ylab     :  string, the y-axis label.
#'      \item main     :  string, the title of the plot. 
#'      \item legend   :  logical, whether to show the legend. Default is TRUE.
#'      \item barlab   :  string, the name of the variable that contains the bar labels. Default is NULL.
#'      \item barlab_at_top : logical, whether to place the labels at the top or middle of the bars or. Default is FALSE, meaning at the middle. 
#'      \item barlab_size   : numeric, the size of the bar label text. Default is 3. 
#'      \item barlab_use_pct: logical, whether to use pct format for the bar labels. Default is FALSE.
#'      \item barpos        : string, "stack" or "dodge", specify to show bars stacked or dodged. Default is "stack".
#'      \item dodged_lab_w  : numeric, the width between the dodged bar labels. Default is 1.
#'      \item dodged_lab_h  : numeric, the height between the dodged bar labels. Default is 1. Often not used.
#'      \item ...      : arguments to be passed to format_as_pct()
#' }
#' 
#' @seealso \code{\link{scale_axis}} for adding different scales to the axes. \code{\link{add_bar_label_pos}} for adding label positions to the input data frame.
#' @export
#' @examples
#' # make some fake data
#' df = read.table(header=TRUE, text='
#' student grade
#' Joe 90
#' Mary 75
#' Alex 50')
#' 
#' # draw a barplot
#' barplt = mk_barplot(df)
#' barplt("student", "grade", "student") 
#' barplt("student", "grade", "student", legend=F) 
#' 
#' # make some fake data
#' df2 = read.table(header=TRUE, text='
#' group level val
#' A      small 1.8
#' A      medium 2.2
#' A      large 1.5
#' B      small 2.0
#' B      medium 2.6
#' B      large 1.0
#' C      small 2.5
#' C      medium 1.3
#' C      large 2.9')
#' 
#' # draw a barplot
#' barplt = mk_barplot(df2)
#' barplt("group", "val", "level") 
#' 
#' # calculate the percentage of the levels within each group
#' library(tidyr)
#' library(dplyr)
#' pct = df2 %>% spread(level, val)
#' temp = pct[, 2:4]
#' pct = cbind(group=pct[, 1], temp / apply(temp, 1, sum))
#' pct = pct %>% gather(level, pct, -group)
#' pct$level = factor(pct$level, levels=c("small", "medium", "large"))
#' 
#' # plot a stacked barplot to display the percentages
#' barplt = mk_barplot(pct)
#' p = barplt("group", "pct", "level")
#' scale_axis(p, "y", use_pct=T)
#' scale_axis(p, "y", use_pct=T, pct_jump=0.1)
mk_barplot = function(df) {
        function(xvar, yvar, fillby, xlab="", ylab="", main="", legend=T, 
                 barlab=NULL, barlab_at_top=F, barlab_size=3, 
                 barlab_use_pct=F, barpos="stack", dodged_lab_w=1, 
                 dodged_lab_h=1, ...) {
                                
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar, 
                                                            fill = fillby)) + 
                        ggplot2::geom_bar(stat = "identity", 
                                          position = barpos) + 
                        ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw()

                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                
                if (!is.null(barlab)) {  
                        if (barlab_use_pct) 
                                df$bar_label = format_as_pct(df[[barlab]], ...)
                        else df$bar_label = df[[barlab]]
                        
                        if (barlab_at_top)
                                barlab_pos = paste(yvar, "pos_top", sep="_")
                        else barlab_pos = paste(yvar, "pos_mid", sep="_")
                        
                        if (barpos != "dodge")
                                p = p + ggplot2::geom_text(data=df, 
                                                           ggplot2::aes_string(
                                                                   label = "bar_label",
                                                                   y = barlab_pos), 
                                                           size = barlab_size)
                        else
                                p = p + ggplot2::geom_text(data=df, 
                                                           ggplot2::aes_string(
                                                                   label = "bar_label",
                                                                   y = barlab_pos,
                                                                   ymax = paste0("max(", barlab, ")")), 
                                                           size = barlab_size,
                                                           position=ggplot2::position_dodge(width=dodged_lab_w, 
                                                                                            height=dodged_lab_h))
                }
                p
        }
}


## old working
# mk_barplot = function(df) {
#         function(xvar, yvar, fillby, xlab="", ylab="", main="", legend=T) {
#                 p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar, 
#                                                             fill = fillby)) + 
#                         ggplot2::geom_bar(stat = "identity") + 
#                         ggplot2::labs(x = xlab, y = ylab, title = main) +
#                         ggplot2::theme_bw()
#                                         
#                 if (!legend) p = p + ggplot2::guides(fill = FALSE)
#                 
#                 p
#         }
# }

