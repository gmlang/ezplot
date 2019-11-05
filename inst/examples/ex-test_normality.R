library(ezplot)
library(dplyr)

f = test_normality(iris)
f("Sepal.Length")
f("Sepal.Length", bins = 50, detrend = F)

f = test_normality(films %>% filter(made_money == "yes"))
f("boxoffice", font_size = 10,
  binwidth = function(x) 2*IQR(x) / (length(x)^(1/3)))

# take log of boxoffice first and then re-run
df = films %>% filter(made_money == "yes") %>% mutate(logbo = log(boxoffice))
f = test_normality(df)
f("logbo")
f("logbo", binwidth = function(x) 2*IQR(x) / (length(x)^(1/3)), detrend = F)
