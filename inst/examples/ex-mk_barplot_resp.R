library(ezplot)
library(dplyr)

g = mk_barplot_resp(films)
p = g("mpaa", "boxoffice", xorder = "descend", font_size = 10,
      label_decimals = 0)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")

# use label_size = 0 to remove labels
g("mpaa", "bo_bt_ratio", fillby = "year_cat", label_size = 0) %>%
        add_labs(ylab = "boxoffice / budget ratio")

df = films %>% count(mpaa, made_money) %>% mutate(pct = n / sum(n))
g = mk_barplot_resp(df)
g("mpaa", "n", label_decimals = 0)
g("mpaa", "pct", xorder = "descend")
g("mpaa", "pct", show_pct = T, label_decimals = 2, font_size = 9) %>%
    add_labs(ylab = NULL,
             title = "There're more R rated films than NC-17, PG and PG-13 combined.",
             subtitle = "Although a substantial portion of the films have mpaa rating info missing.",
             caption = "data were scrapped from imdb.com in 2010")
g("mpaa", "pct", fillby = "made_money", show_pct = T)


df = films %>% count(made_money)
g = mk_barplot_resp(df)
g("made_money", "n", label_decimals = 0)
