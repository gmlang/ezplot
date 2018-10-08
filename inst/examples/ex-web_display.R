library(ezplot)

f = mk_barplot_freq(films)
p = f("mpaa", fillby = "made_money", label_decimals = 0)

# make the barplot web-ready
web_display(p)

