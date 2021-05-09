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
add_labs(p, ylab = "sc") %>% scale_axis(scale = "pct")

plt = mk_scatterplot(films)
p = plt("boxoffice", "budget", alpha=0.5, pt_size=1)
print(p)
scale_axis(p, scale = "dollar")
p = scale_axis(p, scale = "log", nticks = 8)
print(p)
p = scale_axis(p, "x", scale = "log2", nticks = 8)
print(p)

plt = mk_scatterplot(films %>% filter(bo_bt_ratio < 1))
p = plt("votes", "bo_bt_ratio", alpha=0.5, pt_size=1)
print(p)
scale_axis(p, scale = "sqrt")
scale_axis(p, scale = "exp")
scale_axis(p, "x", scale = "log", nticks = 8)
scale_axis(p, "x", scale = "log2")
scale_axis(p, "x", scale = "log10", nticks = 7)
scale_axis(p, "x", scale = "log1p")

plt = mk_boxplot(films)
p = plt(yvar = "budget")
p = add_labs(p, ylab = "budget ($)", title = "Distribution of Budget")
scale_axis(p, scale = "dollar")

dat = tibble(date = as.Date(40100:40129, origin = "1899-12-30"),
             rainfall = rnorm(30, 4))
plt = mk_barplot_resp(dat)
p = plt('date', 'rainfall')
scale_axis(p, 'x', scale = 'date', nticks = 15)

dat = tibble(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
plt = mk_barplot_resp(dat)
plt('trt', 'outcome')
