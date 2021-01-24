#' @title Combines several ggplot objects (plots) into one figure.
#'
#' @description
#' Sometimes we want to put several (ggplot) plots together in one figure for
#' publication. \code{combine_plots} does exactly that, allowing additional
#' parameters to control the alignment and index labels of the individual plots,
#' and etc.
#'
#' @param ... Several ggplot objects. Pass in no more than 6 to avoid crowdness.
#' @param align Specifies whether the plots should be horizontally ("h") or
#' vertically ("v") aligned. Options are "none", "h", "v" (default), and "hv"
#' (align in both directions).
#' @param ncol Number of columns in the plot grid. Default = 2.
#' @param labels List of labels to be added to the plots. You can also set
#' \code{labels="auto"} (default) to auto-generate lower-case labels (a. b. c. d. ...),
#' \code{labels="AUTO"} for upper-case labels (A. B. C. D. ...), or
#' \code{labels='autonum'} for integer labels (1. 2. 3. 4. ...).
#'
#' @return A ggplot object that shows multiple graphs in one figure.
#'
#' @export
#' @examples inst/examples/ex-combine_plots.R
combine_plots = function(..., align = 'v', ncol = 2,
                         labels = 'auto', label_size = 10,
                         title = "Grand Title", title_size=12) {

        if (labels == 'auto')
                labels_txt = paste0(letters[seq_along(list(...))], '.')
        if (labels == 'AUTO')
                labels_txt = paste0(LETTERS[seq_along(list(...))], '.')
        if (labels == 'autonum')
                labels_txt = paste0(seq_along(list(...)), '.')

        fig = cowplot::plot_grid(..., ncol=ncol, align=align,
                                 labels = labels_txt,
                                 label_size = label_size)

        # add grand title
        tit = cowplot::ggdraw() +
                cowplot::draw_label(title, fontface='bold', size=title_size)
        cowplot::plot_grid(tit, fig, ncol = 1,
                           # place title and the combined plots in 1 col
                           # with a 8%-to-100% split of the space
                           rel_heights = c(0.08, 1))
}
