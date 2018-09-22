library(ezplot)
library(dplyr)

plt = mk_densityplot(iris)
plt("Sepal.Length", "Species")

plt = mk_densityplot(films)
plt("rating", "year_cat", font_size = 9) %>% add_labs(title = "Density Plot")
plt("rating", "year") # throws error when yvar is integer or numeric

p = plt("boxoffice", "year_cat")
scale_axis(p, "x", scale = "log10") %>% print()

p = plt("bo_bt_ratio", "year_cat", font_size = 10)
scale_axis(p, "x", scale = "log10") %>% print()
p = plt("bo_bt_ratio", "year_cat", cut_tail = 10^-4) %>%
        add_labs(xlab = "boxoffice / budget ratio")
scale_axis(p, "x", scale = "log10") %>% print()
p = plt("bo_bt_ratio", "year_cat", cut_tail = 10^-1.65) %>%
        add_labs(xlab = "Boxoffice / Budget Ratio")
scale_axis(p, "x", scale = "log10") %>% print()
