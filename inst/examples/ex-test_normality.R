library(ezplot)
library(dplyr)

f = test_normality(iris)
f("Sepal.Length",
  caption_right = 'Sepal length is almost normal except at the left tail.')

f = test_normality(films %>% filter(made_money == "yes"))
f("boxoffice", font_size = 10,
  caption_left = 'Boxoffice is heavily right skewed (with a long right tail.)')

# take log10 of boxoffice and standardize the log10 values
df = films %>% filter(made_money == "yes") %>% mutate(logbo = log10(boxoffice))

# make a histogram of logbo
plt_hist = mk_histdens(df)
plt_hist('logbo', bins = 200) # longer left tail

# test normality
f = test_normality(df)
f("logbo", add_vline_median = TRUE,
  xlab_left = 'log10(boxoffice)',
  title_left = 'CDF of log10(boxoffice)',
  title_right = 'Is log10(boxoffice) normally distributed?',
  caption_left = 'Notice the two curves match well but not exactly.',
  caption_right = 'The middle and right part of log10(boxoffice) \n is close to normal, but the left part is NOT.'
  )


# generate 100 random values from a standard normal distribution
df = data.frame(standard_norm = rnorm(100))
f = test_normality(df)
f('standard_norm', linew = 1, add_vline_median = TRUE, show_label_median = FALSE,
  title_left = 'Empirical CDF of 100 random numbers \n drawn from the standard normal distribution',
  xlab_left = 'x',
  title_right = 'What a normal probability plot looks like.')
