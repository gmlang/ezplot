library(ezplot)
library(tidyr)
library(dplyr)

# ex1
dat = films %>% select(year_cat, budget) %>% group_by(year_cat) %>%
        summarise(mid = median(budget), lwr = min(budget), upr = max(budget))
dat
plt = mk_intervalplot(dat)
title = "Budget Range from 1913 to 2014"
p = plt(xvar="year_cat", yvar="mid", ymin_var="lwr", ymax_var="upr",
        ylab="budget ($)", main=title)
scale_axis(p, scale = "log10")


# ex2 
fit = lm(log10(boxoffice) ~ year_cat, data=films)
pred = predict(fit, films, interval="prediction")

dat = data.frame(year_cat=films$year_cat, pred)

plt = mk_intervalplot(dat)
p = plt("year_cat", "fit", ymin_var="lwr", ymax_var="upr", 
        ylab="predicted log10(budget) ($)", 
        main="Budget Prediction Using year_cat")
p
