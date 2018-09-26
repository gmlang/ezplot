library(ezplot)

# plot boxoffice/budget ratio over the years
plt = mk_lineplot(bo_bt_ratio_by_year)
p = plt("year", "bo_bt_ratio")
p = add_labs(p, xlab = NULL, ylab = "boxoffice/budget ratio", subtitle = NULL,
             title = "Boxoffice/Budget Ratio from 1913 to 2014",
             caption = "data source: IMBD")
print(p)
square_fig(p)
square_fig(p, 2)
square_fig(p, 0.5)
