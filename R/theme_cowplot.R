#' Modified cowplot theme
#'
#' Made right margin big enough to show max x-axis value for horizontal plot.
#' Made no backgroud when using facets.
#' See \url{https://github.com/wilkelab/cowplot/blob/master/R/themes.R} for
#' the original cowplot theme.
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels,
#'                    legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#' @export
theme_cowplot = function(font_size = 14, font_family = "", line_size = .5,
                         rel_small = 12/14, rel_tiny = 11/14,
                         rel_large = 16/14) {

        half_line = font_size / 2
        small_size = rel_small * font_size

        # build off of ggthemes::theme_foundation()
        ggthemes::theme_foundation(font_size, font_family) +
                theme(line = element_line(color = "black", size = line_size,
                                          linetype = 1, lineend = "butt"),
                      rect = element_rect(fill = NA, color = NA,
                                          size = line_size, linetype = 1),
                      text = element_text(family = font_family, face = "plain",
                                          color = "black", size = font_size,
                                          hjust = 0.5, vjust = 0.5, angle = 0,
                                          lineheight = .9, margin = margin(),
                                          debug = FALSE),

                      axis.line = element_line(color = "black",
                                               size = line_size,
                                               lineend = "square"),
                      # axis.line.x = NULL,
                      # axis.line.y = NULL,
                      axis.text = element_text(color = "black",
                                               size = small_size),
                      axis.text.x = element_text(
                              margin = margin(t = small_size / 4), vjust = 1),
                      axis.text.x.top = element_text(
                              margin = margin(b = small_size / 4), vjust = 0),
                      axis.text.y = element_text(
                              margin = margin(r = small_size / 4), hjust = 1),
                      axis.text.y.right = element_text(
                              margin = margin(l = small_size / 4), hjust = 0),
                      axis.ticks = element_line(
                              color = "black", size = line_size),
                      axis.ticks.length = unit(half_line / 2, "pt"),
                      axis.title.x = element_text(
                              margin = margin(t = half_line / 2), vjust = 1),
                      axis.title.x.top = element_text(
                              margin = margin(b = half_line / 2), vjust = 0),
                      axis.title.y = element_text(
                              margin = margin(r = half_line / 2), vjust = 1,
                              angle = 90),
                      axis.title.y.right = element_text(
                              margin = margin(l = half_line / 2), vjust = 0,
                              angle = -90),

                      legend.background = element_blank(),
                      legend.spacing    = unit(font_size, "pt"),
                      # legend.spacing.x  = NULL,
                      # legend.spacing.y  = NULL,
                      legend.margin     = margin(0, 0, 0, 0),
                      legend.key        = element_blank(),
                      legend.key.size   = unit(1.1 * font_size, "pt"),
                      # legend.key.height = NULL,
                      # legend.key.width  = NULL,
                      legend.text       = element_text(size = rel(rel_small)),
                      # legend.text.align = NULL,
                      legend.title      = element_text(hjust = 0),
                      # legend.title.align = NULL,
                      legend.position   = "right",
                      # legend.direction  = NULL,
                      legend.justification = c("left", "center"),
                      # legend.box        = NULL,
                      legend.box.margin =  margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.box.spacing = unit(font_size, "pt"),

                      panel.background  = element_blank(),
                      panel.border      = element_blank(),
                      panel.grid        = element_blank(),
                      # panel.grid.major  = NULL,
                      # panel.grid.minor  = NULL,
                      # panel.grid.major.x = NULL,
                      # panel.grid.major.y = NULL,
                      # panel.grid.minor.x = NULL,
                      # panel.grid.minor.y = NULL,
                      panel.spacing     = unit(half_line, "pt"),
                      # panel.spacing.x   = NULL,
                      # panel.spacing.y   = NULL,
                      panel.ontop       = FALSE,

                      # strip.background = element_rect(fill = "grey80"),
                      strip.background = element_blank(),
                      strip.text = element_text(
                              size = rel(rel_small),
                              margin = margin(half_line / 2, half_line / 2,
                                              half_line / 2, half_line / 2)),
                      # strip.text.x      = NULL,
                      strip.text.y      = element_text(angle = -90),
                      strip.placement   = "inside",
                      # strip.placement.x =  NULL,
                      # strip.placement.y =  NULL,
                      strip.switch.pad.grid = unit(half_line / 2, "pt"),
                      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

                      plot.background   = element_blank(),
                      plot.title = element_text(
                              face = "bold", size = rel(rel_large),
                              hjust = 0, vjust = 1,
                              margin = margin(b = half_line)
                              ),
                      plot.subtitle = element_text(
                              size = rel(rel_small), hjust = 0, vjust = 1,
                              margin = margin(b = half_line)
                              ),
                      plot.caption = element_text(
                              size = rel(rel_tiny), hjust = 1, vjust = 1,
                              margin = margin(t = half_line)
                              ),
                      plot.tag = element_text(
                              face = "bold", hjust = 0, vjust = 0.7
                              ),
                      plot.tag.position = c(0, 1),

                      plot.margin = unit(c(1.5, 2, 1.5, 1.5), "lines")
                      # plot.margin = margin(half_line, half_line,
                      #                      half_line, half_line)

                )
}
