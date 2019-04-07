library(ezplot)

plt = mk_scatterplot(films)

p = plt("budget", "boxoffice") %>% add_labs(xlab = "Budget", ylab = "Boxoffice")
add_lm_line(p)
add_lm_line(p, coef.digits = 8, rr.digits = 3, eq_xpos = "right")
add_lm_line(p, eq_xpos = "right", pval_ypos = 0.8)

p = scale_axis(p, scale = "log10")
p = scale_axis(p, "x", scale = "log10")
ggplot2::ggsave("tmp.pdf", add_lm_line(p, coef.digits = 3, rr.digits = 3))
add_lm_line(p, coef.digits = 3, rr.digits = 3,
            pval_xpos = "left", pval_ypos = "top", vjust = 0)

p = plt("budget", "boxoffice", fillby = "made_money")
add_lm_line(p, coef.digits = 8, rr.digits = 3, eq_xpos = "right",
            pval_ypos = 0.8)
p = scale_axis(p, scale = "log")
p = scale_axis(p, "x", scale = "log")
add_lm_line(p, coef.digits = 3, rr.digits = 3)

p = plt("rating", "bo_bt_ratio", fillby = "made_money") %>%
        add_labs(ylab = "Boxoffice / Budget Ratio")
add_lm_line(p, coef.digits = 3, rr.digits = 3)
p %>% scale_axis(scale = "log10") %>%
        add_lm_line(coef.digits = 3, rr.digits = 3)
