library(ezplot)
library(dplyr)
library(ggplot2)

# --- example 1 --- #

df = data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)), gp = gl(2, 100))
f = mk_cdfplot(df)
f('x') # default geom is 'step'
f('x', geom='point')

range(df$x)
f('x', pad = FALSE) # do not extend the curve to -Inf and +Inf horizontally
f('x', colorby = 'gp')

# --- example 2 --- #

# make fake data
set.seed(123)
n = 200
df = data.frame(model_score = rexp(n=n, rate=1:n),
                obs_set = sample(c("training", "validation"), n, replace=TRUE))
df$model_rank = rank(df$model_score)/n
df$target_outcome = rbinom(n, 1, 1-df$model_rank)

# plot
f = mk_cdfplot(subset(df, target_outcome==1))
f('model_rank', colorby = 'obs_set')
f('model_rank', colorby = 'obs_set', pad = F)

p = f('model_rank', colorby = 'obs_set', add_hline_median = T, legend_title = NULL)
print(p)
p %>% scale_axis(axis = 'y', scale = 'pct') %>%
        scale_axis(axis = 'x', scale = 'pct')
p %>% scale_axis(axis = 'y', scale = 'pct', ydigits = 1) %>%
        scale_axis(axis = 'x', scale = 'pct')
p %>% scale_axis(axis = 'y', scale = 'pct') %>%
        scale_axis(axis = 'x', scale = 'pct', xdigits = 0)

square_fig = p %>% scale_axis(axis = 'y', scale = 'pct') %>%
        scale_axis(axis = 'x', scale = 'pct', xdigits = 0) %>%
        square_fig()
square_fig %>%
        add_labs(xlab = "Model Percentile", ylab = "Percent of Target Outcome",
                 title = "Gain Chart") +
        geom_segment(aes(x=0, y=0, xend=0.9, yend=1),
                     color = "gray", linetype="longdash", size=1)

