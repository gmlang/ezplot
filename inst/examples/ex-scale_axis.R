library(ezplot)
library(dplyr)

# make some data
df = read.table(header=TRUE, text='
IQ score
125 0.86
110 0.72
108 0.64')

plt = mk_scatterplot(df)
p = plt("IQ", "score", pt_size = 2)
print(p)
scale_axis(p, scale = "pct")

# also works if we rename y first and then apply pct scale
p = add_labs(p, ylab = "sc")
scale_axis(p, scale = "pct")

plt = mk_scatterplot(films)
p = plt("boxoffice", "budget", alpha=0.5, pt_size=1)
print(p)
scale_axis(p)
scale_axis(p, scale = "dollar")
p = scale_axis(p, scale = "log")
print(p)
p = scale_axis(p, "x", scale = "log2")
print(p)

plt = mk_scatterplot(films %>% filter(bo_bt_ratio < 1))
p = plt("votes", "bo_bt_ratio", alpha=0.5, pt_size=1)
print(p)
scale_axis(p, scale = "sqrt")
scale_axis(p, scale = "exp")
scale_axis(p, "x", scale = "log")
scale_axis(p, "x", scale = "log2")
scale_axis(p, "x", scale = "log10")
scale_axis(p, "x", scale = "log1p")


plt = mk_boxplot(films)
p = plt(yvar = "budget")
p = add_labs(p, ylab = "budget ($)", title = "Distribution of Budget")
scale_axis(p, axis = "y", scale = "dollar")
