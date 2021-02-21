library(ezplot)

films$mpaa = forcats::fct_explicit_na(films$mpaa) # necessary

f = mk_barploth_freq(films)
unique(films$mpaa)
f("mpaa")
f("mpaa", yorder = "alphanumeric")
f("mpaa", yorder = "ascend")
f("mpaa", yorder = "descend")
f("mpaa", fillby = "made_money", label_decimals = 0)
f("year_cat", font_size = 10)
# use label_size = 0 to remove the bar labels
f("year_cat", fillby = "made_money", yorder = "descend", label_size = 0,
  legend_title = "Is profitable?", legend_pos = "top")
f("made_money", fillby = "year_cat", yorder = "descend", label_size = 0,
  legend_title = NULL, legend_pos = "bottom") %>%
        add_labs(ylab = "made money or not")
f("year_cat", fillby = "mpaa", label_size = 0)

f("mpaa", show_pct = T, yorder = "ascend")
f("mpaa", fillby = "made_money", show_pct = T, yorder = "descend",
  legend_title = "Is profitable?", legend_pos = "left")
f("year_cat", fillby = "made_money", show_pct = T)
f("year_cat", fillby = "mpaa", show_pct = T, label_size = 0)

f = mk_barploth_freq(ggplot2::diamonds)
f("cut", show_pct = T, font_size = 9)
f("cut", show_pct = T, fillby = "clarity", label_size = 0, legend_pos = "top") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p = f("cut", fillby = "clarity", font_size = 11, label_size = 0)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")
