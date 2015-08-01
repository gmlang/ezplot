#' @title Create a figure of 4 plots to show the distribution of a numeric variable.
#' 
#' @description
#' \code{plt_dist()} takes in a data frame and a variable name, and plots the 
#' histogram, boxplot, density curve and qqplot of the variable using the base
#' plot system.
#' 
#' @param dat a data frame or a matrix with colnames.
#' @param varname a string specifying the name of the variable of interest.
#' @export
#' @examples
#' plt_dist(cars, "speed")
plt_dist = function(dat, varname) {
        bluish_green = "#009E73"
        par(mfrow = c(2,2))
        hist(dat[, varname], main="histogram", xlab=varname, probability=TRUE, 
             col=bluish_green)
        boxplot(dat[,varname], outchar=TRUE, main="boxplot", cex=0.7, 
                xlab=varname, col=bluish_green)
        plot(density(dat[,varname]), type="l", main="Smoothed density", lwd=3,
             xlab=varname, ylab="density estimate", col=bluish_green)
        qqnorm(dat[,varname], col=bluish_green, cex=0.9, pch=19)
        qqline(dat[,varname])
        par(mfrow=c(1,1))
}