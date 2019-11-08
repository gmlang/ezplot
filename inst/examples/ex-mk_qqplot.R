library(ezplot)
library(dplyr)

set.seed(0)
smp = data.frame(norm = rnorm(100))
plt = mk_qqplot(smp)
plt("norm") # all points should fall inside of CI band
plt("norm", ci_band_type = "ts", font_size = 10)

set.seed(23)
smp = data.frame(norm = rnorm(100, mean = 50, sd = 2))
plt = mk_qqplot(smp)
plt("norm") # notice the values shown on the x and y axes

plt = mk_qqplot(airquality)
plt("Ozone", dist = "exp", dparams = list(rate = 0.022)) %>%
    add_labs(title = "The mean ozone levels from the airquality dataset is approximately exponential",
             caption = "Theoretical Distribution: Exponential with rate 0.022")

set.seed(2323)
log_bo = sample(log(films$boxoffice), 100)
plt = mk_qqplot(data.frame(log_bo))
plt("log_bo") # all points fall inside of CI band, approximately normal, especially above 12
