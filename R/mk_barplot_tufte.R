#' @title Create a function that draws tufte bar charts.
#' 
#' @description Inspired and modified after Lukasz Piwek's article at 
#' http://motioninsocial.com/tufte/#minimal-barchart
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar, yvar, gpvar, xlab="", ylab="", main="",
#'                pt_size=8, font_size=2.5, ...)}
#' \itemize{
#'      \item xvar     :  string, the x variable.
#'      \item yvar     :  string, the y variable.
#'      \item xlab     :  string, x-axis label.
#'      \item ylab     :  string, y-axis label.
#'      \item main     :  string, title of the plot. 
#'      \item gpvar    :  string, the grouping variable.
#'      \item pt_size  :  numeric, size of points. Default = 8
#'      \item font_size:  numeric, size of text. Default = 2.5
#'      \item ...      :  parameters (base_size or base_family) to pass into 
#'                        theme_slopegraph()
#' }
#' 
#' @export
mk_barplot_tufte = function(df) {
        function(xvar, yvar, xorder="alphanumeric", bar_w=0.25,
                 font_size=14, font_family="serif", show_axis_ticks=F,
                 ybreak_n=5, ybreak_w=1, main="") {
                
                # sort the bars 
                if (xorder == "ascend")
                        df[[xvar]] = reorder(df[[xvar]], df[[yvar]])
                if (xorder == "descend")
                        df[[xvar]] = reorder(df[[xvar]], -df[[yvar]])
                
                # find ymax and ybreaks
                ymax = floor(max(df[[yvar]]))
                ydelta = ymax / ybreak_n
                if (!is_wholenum(ydelta)) ydelta = round(ydelta, 1)
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
                                            lwd=ybreak_w) +
                        ggplot2::ggtitle(main)
                        
                p
        }
}

