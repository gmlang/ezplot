% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_pareto.R
\name{check_pareto}
\alias{check_pareto}
\title{Check if observed data can be modeled by the pareto distribution.}
\usage{
check_pareto(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
\code{function(varname, linew = 0.7, xlab = varname, title_left, title_right,
               subtitle_left, subtitle_right, caption_left, caption_right,
               digits = 2, ...)}
\itemize{
     \item varname. String, name of a continuous variable. Its empirical CDF
     will be plotted along side its complement CDF.
     \item linew. Number, width of the line. Default = 0.7.
     \item xlab. String, x label of the left and the right figures.
     Default is varname.
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
The pareto distribution is a power-law probability distribution that describes
the phenomenon that a large portion of xxx (for example, wealth or income) is
concentrated in a small fraction of the population.
\code{check_pareto} takes in a data frame and returns a function for making
ggplot2 type of CDF and CCDF (on a log10-log10 scale) plots side by side on one
canvas of any continuous variable from the data frame. CCDF standands for
Complement CDF. If CCDF on a log10-log10 scale looks like a straight line, the
observed variable is pareto with a shape parameter equal to -slope, and a
location parameter equal to 10^(intercept / -slope).
}
\examples{
inst/examples/ex-check_pareto.R
}
\seealso{
\code{\link{est_params_pareto}} for how the parameters of the
theoretical pareto distribution are estimated.
}
