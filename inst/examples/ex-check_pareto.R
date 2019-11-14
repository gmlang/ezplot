library(ezplot)
library(dplyr)


# generate 100 random values from a pareto distribution (shape 1, location 2.5)
set.seed(291)
df = data.frame(val = EnvStats::rpareto(100, shape = 1, location = 2.5))
f = check_pareto(df)
f('val', digits = 2, xlab = 'x',
  title_left = 'Empirical CDF of 100 random numbers \n drawn from Pareto(shape = 1, location = 2.5)',
  title_right = 'Their CCDF on log10-log10 scale')


# Does population of cities/towns in the US follow a pareto distribution?
df = pops %>% filter(!is.na(pop)) %>%
        # focus on cities/towns with at least 100 million people
        filter(pop >= 10^5)
f = check_pareto(df)
f('pop', xlab = 'Population (in thousands)',
  title_left = 'CDF of Populations (> 100 million) of US Cities/Towns',
  title_right = 'Do they follow a pareto distribution?',
  linew = 1, digits = 1)
