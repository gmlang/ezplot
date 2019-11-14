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
plt("norm") # the theoretical distributional parameters are estimated via MLE
plt('norm', dparams = list(mean = 50, sd = 2)) # use true parameters

plt = mk_qqplot(airquality)
plt("Ozone", dist = "exp", dparams = list(rate = 0.022)) %>%
    add_labs(title = "The mean ozone levels from the airquality dataset is approximately exponential",
             caption = "Theoretical Distribution: Exponential with rate 0.022")

set.seed(2323)
logbo = sample(log(films$boxoffice), 500)
plt = mk_qqplot(data.frame(logbo))
plt("logbo") # the theoretical distributional parameters are estimated via MLE
plt('logbo', dparams = list(mean = mean(logbo), sd = sd(logbo))) # estimated via sample mean and standard deviation

# what if we standardize the log(boxoffice) first
logbo_standardized = (logbo - mean(logbo)) / sd(logbo)
plt = mk_qqplot(data.frame(logbo_standardized))
plt("logbo_standardized")
plt("logbo_standardized", dparams = list(mean = 0, sd = 1))


