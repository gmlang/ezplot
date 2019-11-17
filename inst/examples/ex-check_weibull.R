library(ezplot)
library(dplyr)

# generate 1000 random values from a weibull distribution of shape 1 and location 2
df = data.frame(x = rweibull(1000, 1, 2))
f = check_weibull(df)
f('val', xlab = 'x',
  title_left = 'Empirical CDF of 100 random numbers\n from Exponential(rate = 0.2)',
  title_right = 'Their CCDF on log-y scale')

# generate 1000 random values from a weibull distribution of shape 2 and location 2
df = data.frame(x = rweibull(1000, 2, 2))
f = check_weibull(df)
f('diffs', linew = 1.2, xlab = 'Minutes between births',
  title_left = 'CDF of interarrival times between births',
  title_right = 'Are these interarrival times exponential?',
  add_vline_median = TRUE, digits = 4)
