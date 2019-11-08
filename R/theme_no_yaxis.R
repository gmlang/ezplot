#' A theme for horizontal ggplot2 plots
#'
#' Same with theme_cowplot except without y-axis or y-ticks
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels,
#' legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#' @export
theme_no_yaxis = function(font_size = 14, font_family = "", line_size = .5,
                          rel_small = 12/14, rel_tiny = 11/14,
                          rel_large = 16/14) {
        theme_cowplot(font_size, font_family, line_size,
                      rel_small, rel_tiny, rel_large) +
                theme(axis.line.y = element_blank(),
                      axis.ticks.y = element_blank()
                      )
}


