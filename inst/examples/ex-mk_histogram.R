library(ezplot)

f = mk_histogram(iris)
f("Sepal.Length")
p = f("Sepal.Length", bins = 100, font_size = 12, legend_pos = "top")
add_labs(p, xlab = "Sepal Length")
f("Sepal.Length", bins = 50, add_vline_mean = F)
f("Sepal.Length", bins = 40, add_vline_median = F, legend_pos = "bottom")

f("Sepal.Length", binw = 0.3)
# we can also specify a function for calculating binwidth
f("Sepal.Length", facet_by = "Species", facet_ncol = 3, font_size = 8,
  binw = function(x) 2*IQR(x) / (length(x)^(1/3)))

f = mk_histogram(films)
f("boxoffice") %>% scale_axis(axis = "x", scale = "log10")
