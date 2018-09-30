<<<<<<< HEAD
#' @title Create a function that draws ggplot2 boxplots
#' 
#' @description 
#' \code{mk_boxplot} takes a data frame as input and returns a function that 
#' can be used to make boxplots using variables in the data frame.
#' 
#' @param df A data frame.
#' @return 
#' \code{function(xvar, yvar, fillby="1", xlab="", ylab="", main="", legend=TRUE, 
#'                add_label=TRUE, lab_at_top=TRUE, vpos=0)}
#' \itemize{
#'      \item xvar     :  string, x variable.
#'      \item yvar     :  string, y variable.
#'      \item xlab     :  string, x-axis label.
#'      \item ylab     :  string, y-axis label.
#'      \item main     :  string, title of the plot. 
#'      \item legend   :  logical, show legend or not. Default = TRUE.
#'      \item add_label:  logical, whether or not to put number of observations as labels. Default is TRUE.
#'      \item lab_at_top: logical, whether to put the labels at the top or bottom. Default is TRUE.
#'      \item vpos     :  number, additional height of the text labels beyond the max y-value of each group.
#' }
#' @export
#' @examples 
#' library(ezplot)
#' plt = mk_boxplot(films)
#' 
#' # plot distributions of budget over the years
#' title1 = "Annual Distribution of Budget from 1913 to 2014"
#' p = plt("year", "budget", ylab="budget", main=title1)
#' p = scale_axis(p, "y", scale="comma")
#' print(p)
#' scale_axis(p, scale="log10")
#' 
#' # plot distributions of boxoffice over the years
#' title2 = "Annual Distribution of Boxoffice from 1913 to 2014"
#' p = plt("year", "boxoffice", ylab="boxoffice", main=title2)
#' start = min(films$year)
#' end = max(films$year)
#' p = scale_axis(p, "y", scale="log10")
#' p = p + ggplot2::scale_x_continuous(limits = c(start, end), 
#'                                     breaks = seq(start, end, 10))
#' print(p)
#' 
#' # plot distributions of boxoffice at each aggregated year level
#' p = plt("year_cat", "boxoffice", ylab="boxoffice")
#' p = scale_axis(p, scale="log10")
#' print(p)
#' 
#' # plot distributions of budget at each aggregated year level
#' p = plt("year_cat", "budget", ylab="boxoffice", legend=F)
#' scale_axis(p, scale="log10")
mk_boxplot = function(df) {
        function(xvar, yvar, fillby="1", xlab="", ylab="", main="", legend=T, 
                 add_label=T, lab_at_top=T, vpos=0) {
                xvar_type = class(df[[xvar]])
                # make plot
                if (xvar_type %in% c("character", "factor"))
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, fill=fillby)) +
                                ggplot2::geom_boxplot() +
                                ggplot2::stat_summary(fun.y=mean, geom="point", shape=5, size=2)
                else
                        p = ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar, group=xvar)) +
                                ggplot2::geom_boxplot(color = cb_color("blue"))
                
                # label with count of observations
                if (add_label) {
                        if (lab_at_top)
                                p = p + ggplot2::stat_summary(fun.data = function(x) 
                                        c(y = max(x) + vpos, label = length(x)), 
                                        geom = "text", size = 5)
                        else 
                                p = p + ggplot2::stat_summary(fun.data = function(x) 
                                        c(y = min(x) + vpos, label = length(x)), 
                                        geom = "text", size = 5)
                }
                
                # annotate plot
                p = p + ggplot2::theme_bw() + 
                        ggplot2::labs(x = xlab, y = ylab, title = main)
                if (!legend)
                        p = p + ggplot2::guides(fill = FALSE)
                
                p
        }
}
=======
#' @title Create a function for making publishable ggplot2 boxplots.
#'
#' @description
#' \code{mk_boxplot} takes a data frame as input and returns a function for
#' making boxplots with any categorical variable from the data frame on the
#' x-axis and any continuous variable on the y-axis. The output function can
#' also produce dodged boxplots when supplied a second categorical variable, a
#' fillby variable.
#'
#' @param df A data frame.
#' @return
#' \code{function(xvar, yvar, fillby = "1", notched = FALSE, label_size = 3,
#'                font_size = 14)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable for x-axis.
#'      \item yvar     :  string, name of a continuous variable for y-axis.
#'      \item fillby   :  string, name of a different categorical variable for
#'                        breaking down the y values of each box. Default = "1",
#'                        meaning no such variable is supplied.
#'      \item notched  :  logical, draw notched boxplots when TRUE;
#'                        otherwise, draw regular boxplots. Default = FALSE.
#'      \item label_size: integer, size of bar label text. Default = 3.
#'      \item font_size : overall font size. Default = 14. The font size of the
#'                        axes and legend text is a fraction of this value.
#' }
#' @export
#' @examples inst/examples/ex-mk_boxplot.R
mk_boxplot = function(df) {
        function(xvar, yvar, fillby = "1", notched = FALSE, label_size = 3,
                 font_size = 14) {

                # --- Prep --- #

                if (any(class(df[[xvar]]) %in% c("integer", "numeric")))
                        stop(paste("The x variable,", paste0(xvar, ","),
                                   "is integer or numeric. Change to factor or character.")
                             )

                # --- Main Plot --- #

                p = ggplot(df, aes_string(xvar, yvar)) +
                        geom_boxplot(aes_string(fill = fillby),
                                     alpha = 0.8, notch = notched)

                # draw mean as spade shape
                p = p + stat_summary(aes_string(group = fillby),
                                     fun.y = mean, geom = "point", size = 1,
                                     shape = 5, position = position_dodge(0.75)
                                     )

                # show number of observations above ymax
                #       (drops NAs and show correct counts automatically)
                get_n = function(x) data.frame(y = max(x),
                                               label = paste("n =", length(x))
                                               )
                p = p + stat_summary(aes_string(group = fillby),
                                     fun.data = get_n, geom = "text",
                                     size = label_size, vjust = -0.8,
                                     position = position_dodge(0.75)
                                     )

                # break y-axis into 10 pieces from ymin to ymax
                ybreaks = pretty(df[[yvar]], n = 10)
                p = p + scale_y_continuous(breaks = ybreaks,
                                           limits = range(ybreaks)
                                           )

                # --- Format Legend --- #

                if (fillby == "1") { # remove legend
                        p = p + guides(color = FALSE, fill = FALSE)
                } else { # use colorblind-friendly colors
                        p = p + ggthemes::scale_fill_tableau("Color Blind")
                }


                # --- Customize Theme --- #

                p + labs(x = NULL, y = yvar) + theme_cowplot(font_size)

        }
}
>>>>>>> v1.0.0
