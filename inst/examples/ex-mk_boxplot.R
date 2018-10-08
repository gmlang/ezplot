library(ezplot)
library(dplyr)

f = mk_boxplot(films)
f("year_cat", "rating", notched = T, font_size = 10)
f("year_cat", "rating", fillby = "made_money", notched = T)
f("year_cat", "rating", fillby = "made_money", legend_title = "Is profitable?",
  notched = T)
f("year_cat", "boxoffice") %>% scale_axis(scale = "log10")
f("year_cat", "budget") %>% scale_axis(scale = "log1p")

f = mk_boxplot(films %>% filter(year %in% 2010:2014))
f("year", "rating", notched = T) # throws error because "year" is integer

# change year to factor first and then run
f = mk_boxplot(films %>% filter(year %in% 2010:2014) %>%
                       mutate(year = factor(year)))
f("year", "rating", notched = T)


f = mk_boxplot(ggplot2::mpg)
f("class", "hwy", fillby = "drv", font_size = 9) %>% add_labs(xlab = "class")
f("year", "cty", fillby = "drv") # throws error because "year" is integer

# change year to character first and then run
mpg = ggplot2::mpg
mpg$year = as.character(mpg$year)
f = mk_boxplot(mpg)
f("year", "cty", fillby = "drv")


df = data.frame(x = rep(c("A", "B"), 5),
                y = c(56, 123, 546, 26, 62, 6, NaN, NA, NA, 15))
f = mk_boxplot(df)
f("x", "y") %>%
        add_labs(ylab = NULL,
                 title = "Demo Title", subtitle = "demo subtitle",
                 caption = "fake data")
