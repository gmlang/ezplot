#' @title Create a function that draws tufte bar charts.
#' 
#' @description Inspired and modified after Lukasz Piwek's article at 
#' http://motioninsocial.com/tufte/#minimal-barchart
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar, yvar, xorder="alphanumeric", bar_w=0.25, font_size=14, 
#'                font_family="serif", show_axis_ticks=F, ybreak_n=5, ybreak_h=1, 
#'                ytick_decimals=0, main="")}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item xorder   :  string, takes on one of three values: "alphanumeric", 
#'                        "ascend", or "descend". It arranges the bars by
#'                        their heights accordingly.
#'      \item bar_w    :  numeric, width of the bars. Default = 0.25.
#'      \item font_size:  numeric, size of text. Default = 14.
#'      \item font_family:  string, default = "serif".
#'      \item show_axis_ticks:  logical, display axis ticks if TRUE. 
#'      \item ybreak_n  :  integer, number of y labels. We'll break the bars at 
#'                         into chunks at these positions. Default = 5.
#'      \item ybreak_h  :  numeric, the height of each break. Default = 1.
#'      \item ytick_decimals  :  integer, the number of decimal places of each
#'                               y tick text shown. Default = 0.
#'      \item main  :  string, title of the plot.                         
#' }
#' 
#' @export
mk_barplot_tufte = function(df) {
        function(xvar, yvar, xorder="alphanumeric", bar_w=0.25,
                 font_size=14, font_family="serif", show_axis_ticks=F,
                 ybreak_n=5, ybreak_h=1, ytick_decimals=0, main="") {
                
                # sort the bars 
                if (xorder == "ascend")
                        df[[xvar]] = reorder(df[[xvar]], df[[yvar]])
                if (xorder == "descend")
                        df[[xvar]] = reorder(df[[xvar]], -df[[yvar]])
                
                # find ymax and ybreaks
                ymax = floor(max(df[[yvar]]))
                ydelta = ymax / ybreak_n
                if (!is_wholenum(ydelta)) ydelta = round(ydelta, ytick_decimals)
                ybreaks = seq(0, ymax, ydelta)[-1]
                
                # plot
                p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) + 
                        ggthemes::theme_tufte(base_size = font_size, 
                                              base_family= font_family,
                                              ticks = show_axis_ticks) + 
                        ggplot2::geom_bar(stat = "identity", fill = "gray",
                                          width = bar_w) + 
                        ggplot2::theme(axis.title = ggplot2::element_blank(),
                                       plot.title = ggplot2::element_text(family=font_family, 
                                                                          # face="bold", 
                                                                          size=font_size+6)) + 
                        ggplot2::scale_y_continuous(breaks=ybreaks) + 
                        ggplot2::geom_hline(yintercept=ybreaks, col="white", 
                                            lwd=ybreak_h) +
                        ggplot2::ggtitle(main)
                        
                p
        }
}

