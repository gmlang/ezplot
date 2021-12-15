library(ezplot)
library(dplyr)

f = mk_forestplot(ests_CIs)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='1',
  font_size = 10, panel_space = .5)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='1',
  font_size = 10, panel_space = .5, scale_x_pct = T)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='1',
  font_size = 10, panel_space = .5, scale_x_pct = T, digits = 1)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='group',
  font_size = 10, panel_space = .5, xlab = 'Point Estimate with 95% CI',
  title = 'Fancy!')

# add 4 new vars:
#   - a factor version of group, with levels sorted in ascending alphanumerical order
#   - a factor version of group, with levels sorted in descending alphanumerical order
#   - a factor version of model, with levels sorted in ascending alphanumerical order
#   - a factor version of model, with levels sorted in descending alphanumerical order
dat = ests_CIs
dat$group_fct_incre = factor(dat[['group']], levels = unique(dat[['group']]))
dat$group_fct_decre = factor(dat[['group']], levels = rev(unique(dat[['group']])))
dat$model_fct_incre = factor(dat[['model']], levels = unique(dat[['model']]))
dat$model_fct_decre = factor(dat[['model']], levels = rev(unique(dat[['model']])))

f = mk_forestplot(dat)

# now see how the facet panels are ordered
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model',
  colorby='group_fct_incre', font_size = 10, panel_space = .5)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model',
  colorby='group_fct_decre', font_size = 10, panel_space = .5)

# now see how the ylabels are ordered
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model_fct_incre',
  colorby='group', font_size = 10, panel_space = .5) # ordered in descending order, opposite of y level order
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model_fct_decre',
  colorby='group', font_size = 10, panel_space = .5) # ordered in ascending order, opposite of y level order

# order both the facet panels and ylabels ascendingly
f1 = f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model_fct_decre',
       colorby='group_fct_incre', font_size = 9, panel_space = .5)

# order both the facet panels and ylabels descendingly
f2 = f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model_fct_incre',
       colorby='group_fct_decre', font_size = 9, panel_space = .5)

# combine f1 and f2
combine_plots(f1, f2)
