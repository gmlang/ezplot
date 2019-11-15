library(ezplot)
library(dplyr)

# --- histograms --- #

f = mk_histdens(iris)
f("Sepal.Length") # draws histogram by default
f("Sepal.Length", bins = 100, font_size = 12, legend_pos = "top") %>%
        add_labs(xlab = "Sepal Length")
f("Sepal.Length", bins = 50, add_vline_mean = F)
f("Sepal.Length", bins = 40, add_vline_median = F, legend_pos = "bottom")

f("Sepal.Length", binwidth = 0.3)
# we can also specify a function for calculating binwidth
f("Sepal.Length", facet_by = "Species", facet_ncol = 3, font_size = 8,
  binwidth = function(x) 2*IQR(x) / (length(x)^(1/3)))

f = mk_histdens(films)
f("boxoffice", bins = 40) %>% # draws histogram by default
        scale_axis("x", scale = "log10") %>% scale_axis()


# --- density plots --- #

f = mk_histdens(iris)
f("Sepal.Length", type = 'density')
f("Sepal.Length", type = 'density', adjust = 0.5,
  font_size = 12, legend_pos = "top") %>% add_labs(xlab = "Sepal Length")
f("Sepal.Length", type = 'density', adjust = 0.3,
  add_vline_median = F, legend_pos = "bottom")
f("Sepal.Length", type = 'density', adjust = 5, add_vline_mean = F)

p = f("Sepal.Length", type = 'density',
      facet_by = "Species", facet_ncol = 3, font_size = 8,
      add_vline_median = F, add_vline_mean = F)
print(p)
scale_axis(p, nticks = 8)

f = mk_histdens(films)
f("boxoffice", type = 'density') %>%
        scale_axis(axis = "x", scale = "log10") %>% scale_axis()

