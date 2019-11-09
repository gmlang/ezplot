library(ezplot)
library(dplyr)

f = test_expdist(iris)
f("Sepal.Length") # not exponential since the CCDF plot is non-linear
f("Sepal.Length", add_vline_median = TRUE)
f("Sepal.Length", add_vline_median = TRUE, show_label_median = FALSE)

f = test_expdist(films)
f("boxoffice", colorby = 'made_money', font_size = 10)

f1 = test_expdist(films %>% filter((made_money == 'no')))
f1("boxoffice", font_size = 10, pad = F, xscale_left = 'log10',
   caption_right = 'Boxoffice is not exponentially distributed at the lower end.')


# generate 1000 random values from a exponential distribution of rate 0.2
df = data.frame(exp_vals = rexp(1000, rate = 0.2))
f = test_expdist(df)
f('exp_vals', pad = F,
  xlab = 'x',
  title_left = 'Empirical CDF of 1000 random numbers \n drawn from Exponential(rate = 0.2)',
  title_right = 'Their CCDF on log-y scale looks like this:')

# Is the interarrival times between births exponential?
df = births %>% filter(!is.na(diffs))
f = test_expdist(df)
f('diffs',
  add_vline_median = TRUE, xlab = 'Minutes between births',
  title_left = 'CDF of interarrival times between births',
  title_right = 'Are these interarrival times exponential?',
  caption_right = 'Not exponential, but can be approximated by an exponential model upto 80 minutes.')
