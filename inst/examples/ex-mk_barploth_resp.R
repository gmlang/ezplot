library(ezplot)
library(dplyr)

g = mk_barploth_resp(films)
p = g("boxoffice", "mpaa", yorder = "ascend", font_size = 10,
      label_decimals = 0)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")
films$boxoffice_in_mil = films$boxoffice / 10^6
g = mk_barploth_resp(films)
g("boxoffice_in_mil", "mpaa", yorder = "descend", font_size = 10) %>%
        add_labs(xlab = "Boxoffice (in millions USD)",
                 title = "Fuel efficiency generally decreases with engine size",
                 subtitle = "Two seaters (sports cars) are an exception ...",
                 caption = "Data from fueleconomy.gov"
                 )

# use label_size = 0 to remove labels
g("bo_bt_ratio", "mpaa", fillby = "year_cat", label_size = 0, legend_title = NULL)

df = films %>% count(mpaa, made_money) %>% mutate(pct = n / sum(n))
g = mk_barploth_resp(df)
g("pct", "mpaa", yorder = "descend")
g("pct", "mpaa", label_decimals = 2, show_pct = T, font_size = 9) %>%
   add_labs(ylab = NULL,
            title = "There're more R rated films than NC-17, PG and PG-13 combined.",
            subtitle = "Although a substantial portion of the films have mpaa rating info missing.",
            caption = "data were scrapped from imdb.com in 2010")
g("pct", "mpaa", fillby = "made_money", show_pct = T)
g("pct", "mpaa", fillby = "made_money", show_pct = T,
  legend_title = "Is profitable?")

df = ggplot2::diamonds %>% count(clarity) %>% mutate(pct = n / sum(n))
g = mk_barploth_resp(df)
g("n", "clarity", label_decimals = 0)
g("pct", "clarity", label_decimals = 2)
g("pct", "clarity", show_pct = T)
