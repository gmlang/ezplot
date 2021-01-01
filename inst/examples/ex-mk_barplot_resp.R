library(ezplot)
library(dplyr)

# --- films data --- #

films$mpaa = forcats::fct_explicit_na(films$mpaa) # necessary

g = mk_barplot_resp(films)
p = g("mpaa", "boxoffice", xorder = "descend", font_size = 10,
      label_decimals = 0)
add_labs(p,
         title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")

# set show_pct = T to make a stacked bar chart
# set label_size = 0 to remove labels
g("year_cat", "budget", fillby = "mpaa", show_pct = T, label_size = 0,
  legend_title = NULL)

# when freq and relative freq are available for a categorical variable,
# use mk_barplot_resp() to plot them.
g = mk_barplot_resp(films %>% count(made_money) %>% mutate(pct = n / sum(n)))
p = g("made_money", "n", label_decimals = 0)
add_labs(p, xlab = 'Made Money?', ylab = 'Frequency')
p = g("made_money", "pct", label_decimals = 0, show_pct = T)
add_labs(p, xlab = 'Made Money?', ylab = 'Relative Frequency')

# --- titanic data --- #

d = as.data.frame(Titanic)
plt = mk_barplot_resp(d)
plt('Class', 'Freq', fillby = 'Survived') # dodged bar chart
plt('Class', 'Freq', fillby = 'Survived', show_pct = T) %>% # stacked bar chart
        add_labs(ylab = 'Relative Frequency')

# once again, when freq and relative freq are directly available,
# use mk_barplot_resp() to plot them.
dat = d %>% group_by(Class, Sex) %>%
        summarise(cnt = sum(Freq), .groups = 'drop_last') %>%
        mutate(pct = cnt / sum(cnt))
g = mk_barplot_resp(dat)
g('Class', 'cnt', fillby = 'Sex') %>% # dodged bar chart
  add_labs(xlab='Class', ylab='Frequency')
# the following two way output the identical stacked bar charts
g('Class', 'cnt', fillby = 'Sex', show_pct = T) %>%
  add_labs(xlab='Class', ylab='Relative Frequency')
g('Class', 'pct', fillby = 'Sex', show_pct = T) %>%
  add_labs(xlab='Class', ylab='Relative Frequency')

