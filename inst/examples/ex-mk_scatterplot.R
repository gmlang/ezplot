library(ezplot)

plt = mk_scatterplot(films)
plt("budget", "boxoffice")
plt("budget", "boxoffice") %>%
        add_labs(subtitle = "Total number of observations: 5,944")
plt("budget", "boxoffice", fillby = "year_cat", alpha = 0.2) %>%
    add_labs(title = "Boxoffice and Budget are related, try log scales",
             caption = "Source: IMDB")

plt("budget", "boxoffice", fillby = "year_cat", alpha = 0.2,
    add_cnt_to_legend = F) %>%
    add_labs(title = "Boxoffice and Budget are related, try log scales",
             caption = "Source: IMDB")
plt("year", "rating", font_size = 10) %>% add_labs(xlab = NULL)
plt("year", "rating", jitter = T) %>% add_labs(xlab = NULL)

df = data.frame(V1 = c(56, 123, 546, 26, 62, 6, NaN, NA, NA, 15),
                V2 = c(21, 231, 5, 5, 32, NA, 1, 231, 5, 200),
                V3 = c(NA, NA, 24, 51, 53, 231, NA, 153, 6, 700),
                V4 = c(2, 10, NA, 20, 56, 1, 1, 53, 40, 5000))
nrow(df)

plt = mk_scatterplot(df)
plt("V1", "V2")
plt("V1", "V3")
