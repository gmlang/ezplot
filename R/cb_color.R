#' @title Colorblind-friendly colors
#' 
#' @description
#' \code{cb_color} looks up the hex string of 9 colorblind-friendly colors given
#' by \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/.}
#' 
#' General advice: for thin lines and small objects, darker blue and orange 
#' are preferable to sky blue and yellow. When combining colors, 
#' Use "warm" and "cool" colors alternatively. When using two warm colors or 
#' two cool colors, put distinct differences in brightness or saturation.
#' Avoid combination of colors with low saturation or low brightness. 
#' 
#' @param name The name of a color or palette. Possible values are 'black', 
#'        "gray", "grey", "orange", "sky_blue", "bluish_green", "yellow",
#'        "blue", "vermilion", "reddish_purple", "cb_gray", "cb_grey", and 
#'        "cb_black"
#' 
#' @return A vector of hex strings
#' 
#' @export
#' 
#' @examples
#' # get single color
#' cb_color("black")
#' cb_color("gray") 
#' cb_color("grey") 
#' cb_color("orange")
#' cb_color("sky_blue")
#' cb_color("bluish_green")
#' cb_color("yellow")   
#' cb_color("blue") 
#' cb_color("vermilion") # red
#' cb_color("reddish_purple")
#' 
#' # get a palette of 8 colors with gray
#' cb_color("cb_gray")
#' cb_color("cb_grey")
#' 
#' # get a palette of 8 colors with black
#' cb_color("cb_black")
cb_color = function(name) {
        black = "#000000"
        gray = grey = "#999999"
        orange = "#E69F00"
        sky_blue = "#56B4E9"
        bluish_green = "#009E73"
        yellow = "#F0E442"
        blue = "#0072B2"
        vermilion = "#D55E00"
        reddish_purple = "#CC79A7"
        cb = c(orange, sky_blue, bluish_green, yellow, blue, vermilion, 
               reddish_purple)
        cb_pair = c(blue, vermilion)
        cb_pair2 = c(blue, reddish_purple)
        cb_pair3 = c(sky_blue, vermilion)
        cb_pair4 = c(sky_blue, reddish_purple)
        cb_pair5 = c(bluish_green, vermilion)
        cb_pair6 = c(bluish_green, reddish_purple)
        switch(name, 
               black = black, gray = gray, grey = grey, orange = orange,
               sky_blue = sky_blue, bluish_green = bluish_green, 
               yellow = yellow, blue = blue, vermilion = vermilion,
               reddish_purple = reddish_purple, cb_gray = c(gray, cb), 
               cb_grey = c(grey, cb), cb_black = c(black, cb),
               cb_pair = cb_pair, cb_pair2 = cb_pair2, cb_pair3 = cb_pair3, 
               cb_pair4 = cb_pair4, cb_pair5 = cb_pair5, cb_pair6 = cb_pair6)
}
