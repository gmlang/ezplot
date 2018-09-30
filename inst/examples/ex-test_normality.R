library(ezplot)
library(dplyr)

f = test_normality(iris)
f("Sepal.Length")
f("Sepal.Length", bins = 50, detrend = F)

f = test_normality(films %>% filter(made_money == "yes"))
f("boxoffice", font_size = 10)

# take log of boxoffice first and then re-run
df = films %>% filter(made_money == "yes") %>% mutate(logbo = log(boxoffice))
f = test_normality(df)
f("logbo")
f("logbo", bins = 100, detrend = F)
