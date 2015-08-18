#' @title Create a function that can be used to make a figure of 4 plots showing
#' the distribution of a continuous variable.
#' 
#' @description
#' \code{plt_dist} takes in a data frame and returns a function that you can pass
#' in a variable name of any continuous variable of the data frame and it'll plot 
#' the histogram, boxplot, density curve and qqplot using the base plot system.
#' 
#' @param dat A data frame or a matrix with colnames.
#' @return
#' \code{function(varname)}
#' \itemize{
#'      \item varname: string, the name of the continuous variable to be plotted
#' }
#' 
#' @export
#' @examples
#' f = plt_dist(cars)
#' f("speed")
#' f("dist")
plt_dist = function(dat) {
        function(varname) {
                sky_blue = cb_color("sky_blue")
                blue = cb_color("blue")
                par(mfrow = c(2,2))
                hist(dat[, varname], main="Histogram", xlab=varname, 
                     probability=TRUE, ylab="density", col=sky_blue)
                boxplot(dat[,varname], outchar=TRUE, main="Boxplot", cex=0.7, 
                        xlab=varname, ylab="value", col=sky_blue)
                plot(density(dat[,varname]), type="l", main="Density Plot", lwd=3,
                     xlab=varname, ylab="density", col=blue)
                qqnorm(dat[,varname], col=blue, cex=0.9, pch=19)
                qqline(dat[,varname])
                par(mfrow=c(1,1))
        }
}