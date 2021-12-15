library(ezplot)
library(dplyr)

# --- films data --- #

films$mpaa = forcats::fct_explicit_na(films$mpaa) # necessary

g = mk_barplot_resp(films)
g("mpaa", "boxoffice", font_size = 10)
g("mpaa", "boxoffice", xorder = "alphanumeric", font_size=10)
p = g("mpaa", "boxoffice", xorder = "descend", font_size = 10,
      pct_label_decimals = 0)
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
p = g("made_money", "n")
add_labs(p, xlab = 'Made Money?', ylab = 'Frequency')
p = g("made_money", "pct", is_y_pct = T)
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


# set `is_y_pct = T` when y-values are percentages
g('Class', 'pct', fillby = 'Sex', is_y_pct = T)
# alternatively, show raw values at the top of the bars and the percentages in the middle
g('Class', 'pct', fillby = 'Sex', raw_label_decimals = 2)
dd = dat %>% group_by(Class) %>% summarise(cnt = sum(cnt), .groups = 'drop') %>%
  mutate(pct = cnt / sum(cnt))
g = mk_barplot_resp(dd)
g('Class', 'pct', is_y_pct = T)

# set `is_y_pct = T` when y-values are percentages
df = read.csv(text="category,pct
               Other,0.09
               South Asian/South Asian Americans,0.12
               Interngenerational/Generational,0.21
               S Asian/Asian Americans,0.25
               Muslim Observance,0.29
               Africa/Pan Africa/African Americans,0.34
               Gender Equity,0.34
               Disability Advocacy,0.49
               European/European Americans,0.52
               Veteran,0.54
               Pacific Islander/Pacific Islander Americans,0.59
               Non-Traditional Students,0.61
               Religious Equity,0.64
               Caribbean/Caribbean Americans,0.67
               Latino/Latina,0.69
               Middle Eastern Heritages and Traditions,0.73
               Trans-racial Adoptee/Parent,0.76
               LBGTQ/Ally,0.79
               Mixed Race,0.80
               Jewish Heritage/Observance,0.85
               International Students,0.87",
              stringsAsFactors = FALSE, sep = ",", header = TRUE)
# change category to factor and order its levels in ascending order of pct
df$category = reorder(df$category, df$pct)
plt = mk_barplot_resp(df)
plt("category", "pct", is_y_pct = T, pct_label_decimals = 0) %>%
  rotate_axis_text(text_angle_x = 90, vjust_x = 0.5)


# you can choose how many decimal points to display for raw values
df = read.csv(text="group,avg_value
A,1.878974923
B,2.022301957
C,2.05136
D,1.767081929
E,1.63550033
F,1.561277385", stringsAsFactors = FALSE, sep=',', header=TRUE)

plt = mk_barplot_resp(df)
plt('group', 'avg_value') # the raw labels have 0 decimals by default
plt('group', 'avg_value', raw_label_decimals = 2) # show 2 decimals to see details




