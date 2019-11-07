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
df = films %>% filter(made_money == "yes") %>% mutate(logbo = log10(boxoffice))
f = test_normality(df)
f("logbo", xlab_left = 'log10(boxoffice)')
f("logbo", detrend = F,
  title_left = 'CDF of log10(boxoffice)',
  xlab_left = 'log10(boxoffice)',
  title_right = 'Is log10(boxoffice) normally distributed?')

# generate 1000 random values from a standard normal distribution
df = data.frame(standard_norm = rnorm(1000))
f = test_normality(df)
f('standard_norm',
  title_left = 'Empirical CDF of 1000 random numbers \n drawn from the standard normal distribution',
  xlab_left = 'x',
  title_right = 'Linear trend implies the normal distribution \n is a good model for the sample data.'
  )
