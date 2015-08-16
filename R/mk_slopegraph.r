#' @title A ggplot2 theme for plotting slopegraphs
#'
#' @param base_size Number, base font size
#' @param base_family String, base font family
#' 
#' @return ggplot2 theme
#' @export
theme_slopegraph = function(base_size = 12, base_family = "") {
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = "black"),
          axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), 
                                              lineheight = 0.9, vjust = 1,
                                              margin = ggplot2::margin(0,0,0,0)),
          axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.8),
                                              margin = ggplot2::margin(0,0,0,0)),
          axis.ticks = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks.length = grid::unit(0, "lines"),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.margin = grid::unit(0.25, "lines"), 
          strip.background = ggplot2::element_blank(),
          strip.text.x = ggplot2::element_text(size = ggplot2::rel(0.8)),
          strip.text.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = ggplot2::rel(1)),
          plot.margin = grid::unit(c(1, 0.5, 0.5, 0.5), "lines"),
          complete=FALSE)
}


#' @title Create a function that draws ggplot2 tufte slopegraphs.
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
mk_slopegraph = function(df) {
        function(xvar, yvar, gpvar, xlab="", ylab="", main="",
                 pt_size = 8, font_size = 2.5, ...) {
                
                idx = df[[xvar]] == head(df[[xvar]], 1)
                ylabs = df[[gpvar]][idx]
                yvals = df[["ypos"]][idx]
                
                p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y="ypos")) +
                        ggplot2::geom_line(ggplot2::aes_string(group=gpvar), 
                                           color="grey80") +
                        ggplot2::geom_point(color="white", size=pt_size) +
                        ggplot2::geom_text(ggplot2::aes_string(label=yvar),
                                           size=font_size) +
                        ggplot2::scale_y_continuous(name="", breaks=yvals, 
                                                    labels=ylabs) +
                        ggplot2::labs(x = xlab, y = ylab, title = main)
                
                p + theme_slopegraph(...)
        }
}