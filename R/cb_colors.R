#' @title Colorblind-friendly color palette
#'
#' @description
#' A palette of 9 colorblind-friendly Hex values developed by Masataka Okabe
#' and Kei Ito.
#'
#' @source \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}
#'
#' @export
#' @examples cb_colors['reddish purple']; names(cb_colors)
cb_colors = setNames(
        c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
          "#0072B2", "#D55E00", "#CC79A7"),
        c('black', 'gray', 'orange', 'sky blue', 'bluish green', 'yellow',
          'blue', 'vermilion', 'reddish purple'))

# --- used internally, not exported --- #

cb_gray  = cb_colors['gray']
cb_black = cb_colors['black']

# more colors, not necessarily colorblind-friendly
color_palette = c("#E64B3599", "#4DBBD599", "#00A08799", "#3C548899",
                  "#F39B7F99", "#8491B499", "#91D1C299", "#DC000099",
                  "#7E614899", "#B09C8599", "#80000099", "#76767699",
                  "#FFA31999", "#8A904599", "#155F8399", "#C1662299",
                  "#8F393199", "#58593F99", "#350E2099",
                  "#386cb0","#f87f01","#7fc97f","#ef3b2c", "#feca01",
                  "#a6cee3","#fb9a99","#984ea3","#8C591D")


