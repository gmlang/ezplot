library(ezplot)
library(dplyr)

films$mpaa = forcats::fct_explicit_na(films$mpaa) # necessary

g = mk_barploth_resp(films)
unique(films$mpaa)
g("boxoffice", "mpaa", font_size = 10)
g("boxoffice", "mpaa", yorder = "alphanumeric", font_size=10)
p = g("boxoffice", "mpaa", yorder = "ascend", font_size = 10)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")

films$boxoffice_in_mil = films$boxoffice / 10^6
g = mk_barploth_resp(films)
g("boxoffice_in_mil", "mpaa", yorder = "descend", font_size = 10) %>%
        add_labs(xlab = "Boxoffice (in millions USD)",
                 title = "Fuel efficiency generally decreases with engine size",
                 subtitle = "Two seaters (sports cars) are an exception ...",
                 caption = "Data from fueleconomy.gov")

# use label_size = 0 to remove labels
g("bo_bt_ratio", "mpaa", fillby = "year_cat", label_size = 0,
  legend_title = NULL, legend_pos = "top")

# use is_x_pct = T when the x values are percentages between 0 and 1
df = films %>% count(mpaa, made_money) %>% mutate(pct = n / sum(n))
g = mk_barploth_resp(df)
g("pct", "mpaa", yorder = "descend", is_x_pct = T, pct_label_decimals = 2,
  font_size = 9) %>%
  add_labs(xlab = 'Relative Frequency (%)',
           title = "There're more R rated films than NC-17, PG and PG-13 combined.",
           subtitle = "Although a substantial portion of the films have mpaa rating info missing.",
           caption = "data were scrapped from imdb.com in 2010")

g("n", "mpaa", fillby = "made_money", legend_pos = "left") %>%
  add_labs(xlab = NULL)
g("n", "mpaa", fillby = "made_money", show_pct = T, legend_pos = "bottom",
  legend_title = "Is profitable?") %>% add_labs(xlab = NULL)

df = ggplot2::diamonds %>% count(clarity) %>% mutate(pct = n / sum(n))
g = mk_barploth_resp(df)
g("n", "clarity", pct_label_decimals = 0) %>% add_labs(xlab = 'Frequency')
g("pct", "clarity", show_pct = T, raw_label_decimals = 2) %>%
        add_labs(xlab = 'Relative Frequency (%)')

# --- titanic data --- #

d = as.data.frame(Titanic)
plt = mk_barploth_resp(d)
plt('Freq', 'Class', fillby = 'Survived') # dodged bar chart
plt('Freq', 'Class', fillby = 'Survived', show_pct = T) %>% # stacked bar chart
  add_labs(xlab = 'Relative Frequency')

# once again, when freq and relative freq are directly available,
# use mk_barplot_resp() to plot them.
dat = d %>% group_by(Class, Sex) %>%
  summarise(cnt = sum(Freq), .groups = 'drop_last') %>%
  mutate(pct = cnt / sum(cnt))
g = mk_barploth_resp(dat)
g('cnt', 'Class', fillby = 'Sex') %>% # dodged bar chart
  add_labs(ylab='Class', xlab='Frequency')

# set `is_y_pct = T` when y-values are percentages and already aggregated
g('pct', 'Class', fillby = 'Sex', is_x_pct = T)
# alternatively, show raw values at the end of the bars and the percentages in the middle
g('pct', 'Class', fillby = 'Sex', raw_label_decimals = 2)


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
plt = mk_barploth_resp(df)
plt("pct", "category", is_x_pct = T, pct_label_decimals = 0)

# you can choose how many decimal points to display for raw values
df = read.csv(text="group,avg_value
A,1.878974923
B,2.022301957
C,2.05136
D,1.767081929
E,1.63550033
F,1.561277385", stringsAsFactors = FALSE, sep=',', header=TRUE)

plt = mk_barploth_resp(df)
plt('avg_value', 'group') # the raw labels have 0 decimals by default
plt('avg_value', 'group', raw_label_decimals = 2) # show 2 decimals to see details

