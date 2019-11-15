library(ezplot)
library(dplyr)

# generate 100 random values from a exponential distribution of rate 0.2
df = data.frame(val = rexp(100, rate = 0.2))
f = check_expdist(df)
f('val', xlab = 'x',
  title_left = 'Empirical CDF of 100 random numbers\n from Exponential(rate = 0.2)',
  title_right = 'Their CCDF on log-y scale')

# Is the interarrival times between births exponential?
df = births %>% filter(!is.na(diffs))
f = check_expdist(df)
f('diffs', linew = 1.2, xlab = 'Minutes between births',
  title_left = 'CDF of interarrival times between births',
  title_right = 'Are these interarrival times exponential?',
  add_vline_median = TRUE, digits = 4)
