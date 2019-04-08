library(dplyr)
library(ezplot)

plt = mk_scatterplot(films)

p = plt("budget", "boxoffice") %>% add_labs(xlab = "Budget", ylab = "Boxoffice")
add_lm_line(p)
add_lm_line(p, eq_tb_xpos = "right", pv_r2_ypos = 0.6)
add_lm_line(p, show = "tb", eq_tb_xpos = "right", pv_r2_ypos = 0.6, digits = 2)

# calc 95% confidence interval of the slope
moe = moe_lm_param(se = 0.16, n = 5944)
ci95 = 4.8 + c(-1, 1) * moe

p = scale_axis(p, scale = "log10")
p = scale_axis(p, "x", scale = "log10")
add_lm_line(p, coef.digits = 3, rr.digits = 3)
add_lm_line(p, show = "tb", pv_r2_xpos = "left", pv_r2_ypos = "top",
            vjust = 1.5, digits = 4)

p = plt("budget", "boxoffice", fillby = "made_money")
add_lm_line(p, coef.digits = 8, rr.digits = 3, eq_tb_xpos = "right")

# Don't show fit table when having a fillby because the fit table is the
# overall one (i.e., the fillby variable doesn't affect the fit table).
add_lm_line(p, show = "tb", digits = 4) # avoid doing this!

p = scale_axis(p, scale = "log")
p = scale_axis(p, "x", scale = "log")
add_lm_line(p)

p = plt("rating", "bo_bt_ratio", fillby = "made_money") %>%
        add_labs(ylab = "Boxoffice / Budget Ratio")
add_lm_line(p, coef.digits = 3, rr.digits = 3)
p %>% scale_axis(scale = "log10") %>% add_lm_line(coef.digits = 3, rr.digits = 3)
