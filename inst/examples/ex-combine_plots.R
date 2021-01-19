library(ezplot)
library(dplyr)

# prep data
df = films %>% mutate(mpaa = forcats::fct_explicit_na(mpaa)) %>%
        group_by(mpaa, made_money) %>%
        summarise(freq = n(),
                  boxoffice = sum(boxoffice) / 1e9,
                  budget = sum(budget) / 1e9,
                  votes = sum(votes) / 1e9,
                  .groups = 'drop')

# make 4 separate plots
f = mk_barplot_resp(df)
p1 = f("mpaa", 'freq', fillby = "made_money", legend_pos = 'top', font_size = 8)
p2 = f("mpaa", 'boxoffice', fillby = "made_money", legend_pos = 'top',
       font_size = 8) %>% add_labs(ylab = 'boxoffice (billion $)')
p3 = f("mpaa", 'budget', fillby = "made_money", legend_pos = 'top',
       font_size = 8) %>% add_labs(ylab = 'budget (billion $)')
p4 = f("mpaa", 'votes', fillby = "made_money", show_pct = T,
       legend_pos = 'top', font_size = 8)

combine_plots(p1, p2, p3, p4, title = 'Some Insights from the Films Data')
