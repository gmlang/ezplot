library(ezplot)
library(dplyr)

f = mk_forestplot(ests_CIs)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='1',
  font_size = 10, panel_space = .5)
f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model', colorby='group',
  font_size = 10, panel_space = .5)

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
       colorby='group_fct_incre', font_size = 10, panel_space = .5)

# order both the facet panels and ylabels descendingly
f2 = f(xvar='est', xmin_var='lwr', xmax_var='upr', yvar='model_fct_incre',
       colorby='group_fct_decre', font_size = 10, panel_space = .5)

combine_plots(f1, f2)

# combine f1 and f2
g1 = ggplot2::ggplot_gtable(ggplot2::ggplot_build(f1))
g2 = ggplot2::ggplot_gtable(ggplot2::ggplot_build(f2))
ab = cbind(g1, g2)
ab$heights = grid::unit.pmax(f1$heights, f2$heights)
# grid::grid.newpage()
png(file.path(out_path, "forest_ab.png"),
    width = 12, height = 8, units = 'in', res = 144)
grid::grid.draw(ab)
dev.off()
tiff(file.path(out_path, "forest_ab.tiff"),
     width = 12, height = 8, units = 'in', res = 144)
grid.draw(ab)
dev.off()
pdf(file.path(out_path, "forest_ab.pdf"), width = 12, height = 8)
grid.draw(ab)
dev.off()
