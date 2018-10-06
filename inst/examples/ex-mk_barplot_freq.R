library(ezplot)

f = mk_barplot_freq(films)
f("mpaa")
f("mpaa", xorder = "descend")
f("year_cat", fillby = "made_money", font_size = 10)
f("mpaa", fillby = "made_money", label_decimals = 0)
f("made_money", fillby = "year_cat")
f("year_cat", fillby = "mpaa")
# use label_size = 0 to remove bar labels
f("year_cat", fillby = "mpaa", label_size = 0)

f("mpaa", show_pct = T, xorder = "descend")
f("mpaa", fillby = "made_money", show_pct = T, xorder = "descend",
  label_decimals = 2)
f("year_cat", fillby = "made_money", show_pct = T)
f("year_cat", fillby = "mpaa", show_pct = T, label_size = 0)

f = mk_barplot_freq(ggplot2::diamonds)
f("cut")
f("cut", show_pct = T)
f("cut", show_pct = T, fillby = "clarity", label_size = 0) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p = f("cut", fillby = "clarity", font_size = 11, label_size = 0)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")