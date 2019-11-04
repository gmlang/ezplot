library(ezplot)
library(dplyr)

f = mk_densityplot(iris)
f("Sepal.Length")
p = f("Sepal.Length", adjust = 0.5, font_size = 12, legend_pos = "top")
add_labs(p, xlab = "Sepal Length")
f("Sepal.Length", adjust = 0.3, add_vline_median = F, legend_pos = "bottom")
f("Sepal.Length", adjust = 5, add_vline_mean = F)
f("Sepal.Length", facet_by = "Species", facet_ncol = 3, font_size = 8,
  adjust = 0.9)


f = mk_densityplot(films)
f("boxoffice") %>% scale_axis(axis = "x", scale = "log10") %>%
        scale_axis(axis = 'y', scale = 'breaks10')

