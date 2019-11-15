library(ezplot)
library(dplyr)

f = check_normality(iris)
f("Sepal.Length", add_vline_median = TRUE,
  caption_right = 'Sepal length is almost normal except at the left tail.')

df = films %>% filter(made_money == "yes", action == 1)
f = check_normality(df)
f("boxoffice", digits = 0, font_size = 12,
  title_left = 'CDF of boxoffice for profitable action films',
  title_right = 'Check normality',
  caption_right = 'right skewed with a long right tail')

# take log of boxoffice
df = df %>% mutate(logbo = log(boxoffice))

# make a histogram of logbo
plt_hist = mk_histdens(df)
plt_hist('logbo', bins = 200) # longer left tail

# check normality of logbo
f = check_normality(df)
f("logbo", xlab_left = 'Boxoffice (log dollars)',
  title_left = 'CDF of log(boxoffice)',
  title_right = 'Is log(boxoffice) normally distributed?',
  caption_left = 'The two curves match well but not exactly.',
  caption_right = 'The middle and right parts are about normal, but the left tail is NOT.'
  )

# generate 100 random values from a standard normal distribution
set.seed(92031)
df = data.frame(standard_norm = rnorm(100))
f = check_normality(df)
f('standard_norm', linew = 1, add_vline_median = TRUE, show_label_median = FALSE,
  title_left = 'CDF of 100 random numbers from Norm(0, 1)',
  xlab_left = 'x', title_right = 'What a normal probability plot looks like.')
