library(ezplot)

tmp = films
tmp$log_budget = log(tmp$budget)
tmp$log_boxoffice = log(tmp$boxoffice)
f = mk_percentileplot_convars(tmp)
f('budget', 'boxoffice')
f('log_budget', 'log_boxoffice')
f('log_budget', 'log_boxoffice', probs = c(0.1, 0.2, 0.5, 0.8, 0.9))
f('log_budget', 'log_boxoffice', xcuts = seq(7, 21, 3))
