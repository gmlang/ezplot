#' @title Color palette that I use a lot in my work.
#' 
#' @description
#' \code{palette()} takes in a color name and returns the corresponding palette.
#' 
#' @param name "cb_gray", "cb_black", "blue", "red", "purple", or "green".
#' 
#' @return a character vector of color palettes
#' 
#' @export
#' 
#' @examples
#' palette("cb_gray")
#' palette("blue")
palette = function(name) {
        # colorblind-friendly palette taken from 
        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        cb = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")
        switch(name, 
               cb_gray = c("#999999", cb), 
               cb_black = c("#000000", cb),
               blue = "#569BBD",
               red = "#CC6666", 
               purple = "#9999CC",
               green = "#66CC99",
               orange = "FA6900", 
               yellow = "F8CA00",
               dark_black = "040004",
               light_black = "413D3D",
               lighter_black = "50535B", # or D3D4D8
               dark_blue = "0B486B",
               light_blue = "69D2E7")
}


        