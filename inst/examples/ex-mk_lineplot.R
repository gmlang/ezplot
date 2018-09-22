library(ezplot)
library(dplyr)

# plot boxoffice/budget ratio over the years
plt = mk_lineplot(bo_bt_ratio_by_year)
p = plt("year", "bo_bt_ratio")
add_labs(p, xlab = NULL, ylab = "boxoffice/budget ratio",
         title = "Boxoffice/Budget Ratio from 1913 to 2014",
         caption = "data source: IMBD")

# plot total budget and boxoffice over the years
plt = mk_lineplot(btbo_by_year)
p = plt("year", "tot", fillby = "type", font_size = 10, add_cnt_to_legend = F)
p = add_labs(p, xlab = NULL, ylab = "total amount ($billion)",
             title = "Annual Total Budget and Boxoffice from 1913 to 2014",
             subtitle = NULL)
scale_axis(p, scale = "log") %>% print()


plt = mk_lineplot(films %>% group_by(year_cat) %>%
                  summarise(avg_boxoffice = mean(boxoffice)))
p = plt("year_cat", "avg_boxoffice")
add_labs(p, xlab = NULL, ylab = "Mean Boxoffice", subtitle = "haha")
