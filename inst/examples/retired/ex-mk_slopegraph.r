# load libraries
library(readr)
library(ezplot)

# set path
proj_path = "~/Rpkg/ezplot-tests"
data_path = file.path(proj_path, "cancer_survival_rates.csv")
dat = read_csv(data_path)

# prep data for making tufte slopegraph
df = slopegraph_data_prep(dat, xvar="year", yvar="value", gpvar="group")

# change the x variable to factor, make sure the time order is ascending, 
# label it so that it's human readable, round the y values.
df = within(df, { year = factor(year, levels=c(5,10,15,20), 
                                labels=c("5 years", "10 years", 
                                         "15 years", "20 years"))
                  value=round(value) })

# Generate the raw plot
plt = mk_slopegraph(df) 
gg = plt("year", "value", "group", main="Estimates of % survival rates") 
gg

# Save the results
w = 6  	# width in inches
h = 1.6 * w	# height in inches
ggplot2::ggsave(file.path(proj_path, "slopegraph.pdf"), gg, w=w, h=h)

dpi = 150  	# resolution for png
ggplot2::ggsave(file.path(proj_path, "slopegraph.png"), gg, w=w, h=h, dpi=dpi)
