library(ezplot)

f = mk_barplot_freq(films)
p = f("mpaa", fillby = "made_money", label_decimals = 0)

# make the barplot web-ready
web_display(p)
web_display(p, legend_title = FALSE)
web_display(p, legend_pos = "none")
web_display(p, legend_pos = "top")
web_display(p, legend_pos = "bottom")

