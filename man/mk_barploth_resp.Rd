% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mk_barploth_resp.R
\name{mk_barploth_resp}
\alias{mk_barploth_resp}
\title{Create a function for making ggplot2 horizontal bar charts, showing
the sums of a continuous variable by 1 or 2 categorical variables.}
\usage{
mk_barploth_resp(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
\code{function(xvar, yvar, fillby = "1", yorder = NULL, is_x_pct = FALSE,
               show_pct = FALSE, pct_label_decimals=1, raw_label_decimals=0,
               label_size = 3, legend_title = fillby, legend_pos = "right",
               font_size = 14)}
\itemize{
     \item xvar String. Name of a continuous variable for x-axis.
     \item yvar String. Name of a categorical variable for y-axis.
     \item fillby String. Name of a different categorical variable for
     subdividing and coloring the bars. Default = "1", meaning no such
     variable is supplied.
     \item yorder String. Possible values: NULL (default), "alphanumeric",
     "ascend" or "descend". It specifies how categories are ordered on the
     y-axis. When NULL, the categories are shown in their order in the data.
     \item is_x_pct. Logical, if TRUE, format x-axis as %. Default is FALSE.
     Set it to TRUE when the input xvar has values between 0 and 1 that can
     also be viewed as percents.
     \item show_pct. Logical, if TRUE, calculate the relative frequencies of
     the aggregated y values between each x category, and then display them
     as percentages on y-axis and also format bar labels as %; otherwise,
     format them as comma. Default is FALSE.
     \item pct_label_decimals. Integer, number of decimal points shown for
     the percent labels. Default = 1.
     \item raw_label_decimals. Integer, number of decimal points shown for
     the raw number labels. Default = 0.
     \item label_size Integer. The size of bar label text. Default = 3. If 0,
     bar labels will not be shown.
     \item legend_title String. Legend title. Default is the name of the
     `fillby` variable.
     \item legend_pos String. Legend position. Possible values are
     "right" (default), 'left', 'top' and 'bottom'.
     \item font_size Number. Overall font size (default = 14). The font size
     of the axes and legend is a fraction of this value.
}
}
\description{
\code{mk_barploth_resp} takes a data frame as input and returns a function
for making bar charts with any categorical variable from the data frame on
the y-axis and the sums of any continuous variable on the x-axis. The output
function can also produce dodged bar charts when supplied a second categorical
variable. The resulting bar chart will have bars ordered by the alphanumerical
order of the y levels by default, and labeled by the sums of the x values.
Users can specify the number of decimal places to show on the bar labels. If
the sums of x values are between 0 and 1, users can choose to format the
x-axis and the bar labels as percent (%). Plus, the resulting figure has a
clean theme and font, and uses color-blind friendly palettes.
}
\examples{
inst/examples/ex-mk_barploth_resp.R
}
\seealso{
\code{\link{get_bar_labels_resp}} for how to calculate the positions
         of the bar labels.
}
