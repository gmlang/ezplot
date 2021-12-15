#' @title Create a function for making publishable ggplot2 forest plot.
#'
#' @description
#' \code{mk_forestplot} takes a data frame as input and returns a function for
#' making forest plots that can display estimates and their confidence intervals
#' of different groups and subgroups. Forest plots make it easy to compare
#' between groups.
#'
#' @param df A data frame where each row represents a group, subgroup or model
#' and etc. For example, say you trained 10 simple linear regressions, each using
#' a different feature, and collected the coefficient estimates and their 95\%
#' confidence intervals into a data frame. You want to see the difference between
#' these coefficient estimates. You can call \code{mk_forestplot} on this
#' data frame of coefficient estimates and 95\% confidence intervals.
#'
#' @return
#' \code{function(xvar, xmin_var, xmax_var, yvar, colorby = '1', panel_space = 1,
#'                strip_text_y_margin=c(2, 3, 2, 3), plot_margin=c(2, 1, 2, 1),
#'                font_size=14)}
#' \itemize{
#'      \item xvar. String, name of a continuous var of estimates or effect sizes.
#'      \item xmin_var. String, name of a continuous var of the lower bounds of
#'      those estimates, for example, 95\% lower bound.
#'      \item xmax_var. String, name of continuous var of the upper bounds of
#'      those estimates, for example, 95\% upper bound.
#'      \item yvar. String, name of a categorical var for the left y-axis. Each
#'      value should corresponds to one row from the data. The data should not
#'      contain repeated rows.
#'      \item colorby. String, name of a categorical variable for faceting and
#'      all line segments within the same facet will have the same color.
#'      Default = "1", meaning no such variable is supplied.
#'      \item panel_space. Number, space between the facets. Default = 1.
#'      \item add_vline_xpos. Number, the x position (default = 0) where to draw
#'      a vertical dashed line.
#'      \item strip_text_y_margin. Vector of 4 numbers, sets the position of
#'      the facet panel text relative to the boundry of strips of facet panels.
#'      Default = c(2, 3, 2, 3), going from top, right, bottom, to left.
#'      \item plot_margin. Vector of 4 numbers, sets the position of the plot
#'      relative to canvas. Default = c(2, 1, 2, 1), going from top, right,
#'      bottom, to left.
#'      \item font_size. Overall font size. Default = 14. The font size of the
#'      axes and legend text is a fraction of this value.
#'      \item xlab. A string for labeling the x-axis. When NULL (default), the
#'      value of xvar is used. This parameter is added to the function because
#'      the object returned from the function cannot be modified by add_labs().
#'      \item title. A string as the title of the plot. When NULL (default), the
#'      plot is untitled. This parameter is added to the function because
#'      the object returned from the function cannot be modified by add_labs().
#'      \item scale_x_pct. Logical. If TRUE, use % scale on the x-axis so that
#'      the x-labels are all %. Default is FALSE.
#'      \item digits. Number of digits when using percent format on x-axis.
#       Default is NULL. Only valid when scale_x_pct is TRUE.
#' }
#'
#' @export
#' @examples inst/examples/ex-mk_forestplot.R
mk_forestplot = function(df) {
        function(xvar, xmin_var, xmax_var, yvar, colorby = '1', panel_space = 1,
                 add_vline_xpos = 0, strip_text_y_margin = c(2, 3, 2, 3),
                 plot_margin = c(2, 1, 2, 1), font_size = 14, xlab = NULL,
                 title = NULL, scale_x_pct = FALSE, digits) {

                # --- set up --- #

                if (colorby == '1') {
                        # use black for all line segments
                        pals = 'black'
                } else {
                        # colorby is used to facet, so we get its levels
                        gpvec = df[[colorby]]
                        lvls = unique(gpvec)
                        nfacets = length(lvls)

                        # set colors for the line segments and facet strips
                        cpalette = color_palette[1:nfacets]
                        # scales::show_col(cpalette)
                        pals = rep(cpalette, table(gpvec))
                }

                # make x-axis breaks
                xaxis_breaks = pretty(c(df[[xmin_var]], df[[xmax_var]]), 10)

                # helper function
                if (missing(digits)) {
                        to_pct = scales::percent
                } else {
                        to_pct = function(v) scales::percent(v, accuracy = 10^(-digits))
                }

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, yvar, xmin=xmin_var, xmax=xmax_var)) +
                        # add estimates as dots
                        geom_point(alpha = 0.8) +
                        # add CIs as error bars with color determined by colorby
                        geom_errorbarh(aes_string(xmin=xmin_var, xmax=xmax_var),
                                       col = pals, height = 0.4, size = 0.8) +
                        # add vertical line at 0
                        geom_vline(xintercept=add_vline_xpos,
                                   linetype="dashed", alpha=.5)

                if (scale_x_pct) {
                        p = p + scale_x_continuous(
                                # limits = range(xaxis_breaks),
                                breaks = xaxis_breaks,
                                labels = to_pct)

                } else {
                        p = p + scale_x_continuous(
                                limits = range(xaxis_breaks),
                                breaks = xaxis_breaks)

                }

                # --- Facet when colorby is not '1' --- #

                if (colorby != '1')
                        # facet based on colorby
                        p = p + facet_grid(reformulate('.', colorby),
                                           scales = "free_y", space = "free_y")

                # --- Customize Theme --- #

                if (is.null(xlab))
                        xlab = paste(xvar, 'with Lower and Upper Bounds')
                p = p + labs(x=xlab, y=NULL, title=title) +
                        # apply the bw theme, necessary
                        theme_bw(base_size = font_size) +
                        # further customize the theme
                        theme(axis.ticks        = element_blank(),
                              panel.background  = element_blank(),
                              panel.border      = element_blank(),
                              plot.background   = element_blank(),
                              legend.position   = "none", # removes the legend

                              # must comment this out, cannot remove strip
                              # background because doing so leads to error later
                              # when we apply background fill using our own
                              # colors to the strips.
                              # strip.background  = element_blank(),

                              plot.title = element_text(face = "bold",
                                                        vjust = 1,
                                                        hjust = 0.5,
                                                        lineheight = 0.5),
                              axis.title.x = element_text(face = "bold",
                                                          hjust = 0.5,
                                                          vjust = -2),
                              axis.text.x = element_text(face = "bold",
                                                         hjust = 0.5,
                                                         vjust = 0.1,
                                                         color = 'black'),
                              axis.text.y = element_text(face = "bold",
                                                         color = 'black'),
                              strip.text.y = element_text(
                                      face = "bold",
                                      # set the position of the text relative to
                                      # the boundry of strips of facet panels
                                      margin = margin(strip_text_y_margin)),

                              # set spaces between consecutive facet panels
                              panel.spacing = unit(panel_space, "lines"),
                              panel.grid.minor.y = element_blank(),

                              # set the position of the plot relative to canvas
                              plot.margin = unit(plot_margin, units = "cm"))

                # fill facet strip backgrounds with the corresponding colors of
                # the lines
                g = ggplot_gtable(ggplot_build(p))
                stripr = which(grepl('strip-r', g$layout$name))
                k = 1
                for (i in stripr) {
                        j = which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
                        g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill = unique(pals)[k]
                        k = k+1
                }

                # convert the grob object back to a ggplot object and return it
                ggpubr::as_ggplot(g)
        }
}



