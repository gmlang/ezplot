library(ezplot)
library(dplyr)
library(tidyr)

# plot boxoffice/budget ratio over the years
plt = mk_lineplot(bo_bt_ratio_by_year)
p = plt("year", "bo_bt_ratio")
add_labs(p, xlab = NULL, ylab = "boxoffice/budget",
         title = "Boxoffice/Budget from 1913 to 2014",
         caption = "data source: IMBD")

# plot total budget and boxoffice over the years
plt = mk_lineplot(btbo_by_year)
plt("year", "tot", colorby = "type")
plt("year", "tot", colorby = "type", legend_title = NULL, legend_pos = "bottom")
p = plt("year", "tot", colorby = "type", legend_title = NULL, legend_pos = "top",
        add_cnt_to_legend = F, font_size = 10)
p = add_labs(p, xlab = NULL, ylab = "total amount ($billion)",
             title = "Annual Total Budget and Boxoffice from 1913 to 2014",
             subtitle = NULL)
scale_axis(p, scale = "log")

plt = mk_lineplot(films %>% group_by(year_cat) %>%
                  summarise(avg_boxoffice = mean(boxoffice), .groups='drop'))
p = plt("year_cat", "avg_boxoffice")
p %>% add_labs(xlab = NULL, ylab = "Mean Boxoffice", subtitle = "haha") %>%
        scale_axis(scale = 'dollar')


# make rank-order plot
df_wide = data.frame(parts = c('Engine', 'Electrical System', 'Brakes',
                               'Air Conditioning', 'Transmission', 'Body Integrity'),
                     indpct = c(0.43, 0.3, 0.12, 0.09, 0.05, 0.01),
                     cumpct = c(0.43, 0.73, 0.85, 0.94, 0.99, 1.00))
plt = mk_lineplot(df_wide)
plt('parts', 'cumpct', xorder = 'ascend', is_y_pct = T)
plt('parts', 'indpct', xorder = 'descend', is_y_pct = T)

# put both cumulative (lift curve) and individual curves on one graph.
parts_lvls = c('Engine', 'Electrical System', 'Brakes', 'Air Conditioning',
               'Transmission', 'Body Integrity') # necessary to order the x categories
df_long = df_wide %>%
        pivot_longer(!parts, names_to = 'type', values_to = 'pct') %>%
        mutate(parts = factor(parts, levels = parts_lvls),
               type = gsub('indpct', 'individual', type),
               type = gsub('cumpct', 'cumulative', type))
f = mk_lineplot(df_long)
f('parts', 'pct', colorby = 'type', is_y_pct = T, legend_title = NULL, legend_pos = 'top')
