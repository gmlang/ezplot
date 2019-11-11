library(ezplot)
library(dplyr)

f = test_normality(iris)
f("Sepal.Length", add_vline_median = TRUE,
  caption_right = 'Sepal length is almost normal except at the left tail.')

df = films %>% filter(made_money == "yes")
f = test_normality(df)
f("boxoffice", font_size = 10,
  caption_left = 'Boxoffice is heavily right skewed (with a long right tail.)')

# take log10 of boxoffice
df = df %>% mutate(log10bo = log10(boxoffice))

# make a histogram of log10bo
plt_hist = mk_histdens(df)
plt_hist('log10bo', bins = 200) # longer left tail

# test normality
f = test_normality(df)
f("log10bo", xlab_left = 'Boxoffice (log10 dollars)',
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
