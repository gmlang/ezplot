library(ezplot)
library(dplyr)

f = test_expdist(iris)
f("Sepal.Length") # not exponential since the CCDF plot is non-linear
f("Sepal.Length", add_vline_median = TRUE)
f("Sepal.Length", add_vline_median = TRUE, show_label_median = FALSE)

f = test_expdist(films %>% filter(made_money == "yes"))
f("boxoffice", font_size = 10) # not exponential since the CCDF plot is non-linear


# generate 1000 random values from a exponential distribution of rate 0.2
df = data.frame(exp_vals = rexp(1000, rate = 0.2))
f = test_expdist(df)
f('exp_vals') # data from an exponential distribution have a linear CCDF plot

# Is the interarrival times between births exponential?
df = births %>% filter(!is.na(diffs))
f = test_expdist(df)
f('diffs', add_vline_median = TRUE,
  title_left = 'CDF of interarrival times between births',
  title_right = 'Are the sample interarrival times exponential?',
  xlab = 'Minutes between births') # more or less exponential upto 50 minutes


