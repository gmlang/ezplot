#' @title Add x, y labels, title, subtitle and caption to a plot.
#'
#' @description
#' \code{add_labs} takes a ggplot object as input and adds to it user-supplied
#' x, y labels, plot title, subtitle and caption. If user doesn't provide
#' values to these parameters, it'll just use the values from the input plot
#' without making changes.
#'
#' @param p A ggplot object.
#' @param xlab String, the x-axis label.
#' @param ylab String, the y-axis label.
#' @param title String, title of the plot. You should use the title to concisely
#' state the main insight the plot reveals. Avoid using generic statement as
#' plot title. For example, "Income vs. Employment Length" is a bad title
#' because it just re-state what the y and x axes are.
#' @param subtitle String, subtitle of the plot. You can use the subtitle to
#' explain the details of the insight. You can even write one or more short
#' paragraphs if you have to.
#' @param caption String, caption of the plot. You can put anything else useful
#' in caption. For example, data source, method referece, url and etc.
#'
#' @return A ggplot object annotated with xy labels, title, subtitle and caption.
#'
#' @export
#' @examples inst/examples/ex-add_labs.R
add_labs = function(p, xlab, ylab, title, subtitle, caption) {

        if (missing(xlab)) xlab = p$labels$x
        if (missing(ylab)) ylab = p$labels$y
        if (missing(title)) title = p$labels$title
        if (missing(subtitle)) subtitle = p$labels$subtitle
        if (missing(caption)) caption = p$labels$caption

        p + ggplot2::labs(x = xlab, y = ylab, title = title,
                          subtitle = subtitle, caption = caption)
}


