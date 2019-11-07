library(ezplot)
library(dplyr)

f = test_normality(iris)
f("Sepal.Length")
f("Sepal.Length", add_vline_median = TRUE, detrend = FALSE)
f("Sepal.Length", add_vline_median = TRUE, show_label_median = FALSE,
  detrend = FALSE)

f = test_normality(films %>% filter(made_money == "yes"))
f("boxoffice", font_size = 10)

# take log of boxoffice first and then re-run
df = films %>% filter(made_money == "yes") %>% mutate(logbo = log(boxoffice))
f = test_normality(df)
f("logbo")
f("logbo", detrend = F)

# generate 1000 random values from a standard normal distribution
df = data.frame(standard_norm = rnorm(1000))
f = test_normality(df)
f('standard_norm')
