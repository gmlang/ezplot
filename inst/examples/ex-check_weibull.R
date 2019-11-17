library(ezplot)
library(dplyr)

# generate 1000 random values from weibull(shape=0.5, location=2)
# generate 1000 random values from weibull(shape=1, location=2)
# generate 1000 random values from weibull(shape=2, location=2)
set.seed(29120)
df = data.frame(x1 = rweibull(1000, 0.5, 2), # failure rate decreases over time
                x2 = rweibull(1000, 1, 2), # failure rate is constant over time, weibull reduces to exponential
                x3 = rweibull(1000, 2, 2)  # failure rate increases over time
                )


f = check_weibull(df)
f('x1',
  title_left = 'Empirical CDF of 1000 random numbers\n from Weibull(k=0.5, lambda=2)',
  title_right = 'Their -logCCDF on log-log scale')

f('x2', add_vline_median = TRUE, digits = 1,
  title_left = 'Empirical CDF of 1000 random numbers\n from Weibull(k=1, lambda=2)',
  title_right = 'Their -logCCDF on log-log scale')

f('x3', add_vline_median = TRUE, digits = 1,
  title_left = 'Empirical CDF of 1000 random numbers\n from Weibull(k=2, lambda=2)',
  title_right = 'Their -logCCDF on log-log scale')
