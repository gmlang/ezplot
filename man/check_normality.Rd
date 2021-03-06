% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_normality.R
\name{check_normality}
\alias{check_normality}
\title{Check if observed data can be modeled by the normal distribution.}
\usage{
check_normality(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
\code{function(varname, linew = 0.7, xlab_left = varname, title_left,
               title_right, subtitle_left, subtitle_right,
               caption_left, caption_right, digits = 2, ...)}
\itemize{
     \item varname. String, name of a continuous variable. Its empirical CDF
     will be plotted along side its normal probability plot.
     \item linew. Number, width of the line. Default = 0.7.
     \item xlab_left. String, x label of the left figure. Default is varname.
     \item title_left. String, title of the left figure.
     \item title_right. String, title of the right figure.
     \item subtitle_left. String, subtitle of the left figure.
     \item subtitle_right. String, subtitle of the right figure.
     \item caption_left. String, caption of the left figure.
     \item caption_right. String, caption of the right figure.
     \item digits. Integer, the number of digits after the decimal point
     for the estimated parameter values of the theoretical distribution.
     Default = 2.
     \item .... Other parameters for making a CDF plot. A common one, for
     example, is `add_vline_median = TRUE`, which will add a vertical line at
     the median. Another common one is `show_label_median = FALSE`, which
     will suppress the display of median value along the median vline. See
     \code{\link{mk_cdfplot}} for a full list of parameters.
}
}
\description{
The normal distribution is the most commonly used probability distribution.
Many real world data such as height can be modeled via the normal distribution.
\code{check_normality} takes in a data frame and returns a function for making
ggplot2 type of CDF plot and the normal probability plot side by side
on one canvas of any continuous variable from the data frame. If normal
probability plot is linear, the observed data can be modeled by the normal
distribution with mean and standard deviation equal to the sample mean and
standard deviation of the observed data.
}
\examples{
inst/examples/ex-check_normality.R
}
