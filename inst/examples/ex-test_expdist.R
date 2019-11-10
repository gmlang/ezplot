library(ezplot)
library(dplyr)


# generate 1000 random values from a exponential distribution of rate 0.2
df = data.frame(exp_vals = rexp(1000, rate = 0.2))
f = test_expdist(df)
f('exp_vals', pad = F, xlab = 'x',
  title_left = 'Empirical CDF of 1000 random numbers \n drawn from Exponential(rate = 0.2)',
  title_right = 'Their CCDF on log-y scale looks like this:')


# Is the interarrival times between births exponential?
df = births %>% filter(!is.na(diffs))
f = test_expdist(df)
f('diffs', add_vline_median = TRUE, xlab = 'Minutes between births',
  linew = 1.5,
  title_left = 'CDF of interarrival times between births',
  title_right = 'Are these interarrival times exponential?')
