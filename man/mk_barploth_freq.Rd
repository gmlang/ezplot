% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mk_barploth_freq.R
\name{mk_barploth_freq}
\alias{mk_barploth_freq}
\title{Create a function for making publishable ggplot2 horizontal barplots,
showing frequency counts (or percentages) of a categorical variable or 2
categorical variables.}
\usage{
mk_barploth_freq(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
\code{function(yvar, fillby = "1", yorder = NULL, show_pct = FALSE,
               pct_label_decimals = 1, label_size = 3, legend_title = fillby,
               legend_pos = "right", font_size = 14)}
\itemize{
     \item yvar. String, name of a categorical variable for y-axis.
     \item fillby. String, name of a different categorical variable for
     subdividing and coloring the bars. Default is "1", meaning no such
     variable is supplied.
     \item yorder. String, Possible values: NULL (default), "alphanumeric",
     "ascend" or "descend". It specifies how categories are ordered on the
     y-axis. When NULL, the categories are shown in their order in the data.
     \item show_pct. Logical, if TRUE, show percent on y-axis; otherwise,
     show count. Default is FALSE.
     \item pct_label_decimals. Integer, number of decimal points shown for
     the percent labels. Default = 1.
     \item label_size. Integer, size of bar label text. Default is 3. Hide
     bar labels when its value is 0.
     \item legend_title. String, legend title. Default is the name of the
     fillby variable.
     \item legend_pos. String, legend position. Default = "right".
     \item font_size. Overall font size. Default = 14. The font size of the
     axes and legend text is a fraction of this value.
}
}
\description{
\code{mk_barploth_freq} takes a data frame as input and returns a function
for making horizontal bar charts with any categorical variable from the data
frame on the y-axis and frequency counts (or percents) of its levels on the
x-axis. The output function can also produce dodged (for counts) or stacked
(for percents) bar charts when supplied a second categorical variable, a
fillby variable. The resulting bar chart will have bars ordered by the
alphanumerical order of the y levels by default, and bars
labeled with both counts and percents. Plus,
it'll have a clean theme and clear fonts.
It'll also use color-blind friendly palettes.
}
\examples{
inst/examples/ex-mk_barploth_freq.R
}
\seealso{
\code{\link{scale_axis}} for adding different scales to the y-axis.
}
