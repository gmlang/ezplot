library(ezplot)
library(dplyr)

g = mk_lollipop(films)
p = g("boxoffice", "mpaa", yorder = "descend", font_size = 10)
add_labs(p, title = "Fuel efficiency generally decreases with engine size",
         subtitle = "Two seaters (sports cars) are an exception ...",
         caption = "Data from fueleconomy.gov")

df = films %>% count(mpaa, made_money) %>% mutate(pct = n / sum(n))
g = mk_lollipop(df)
g("pct", "mpaa")
g("pct", "mpaa", label_decimals = 1, yorder = "descend")

# use label_size = 0 to remove label text
g("pct", "mpaa", show_pct = T, font_size = 9, label_size = 0) %>%
   add_labs(ylab = NULL,
            title = "There're more R rated films than NC-17, PG and PG-13 combined.",
            subtitle = "Although a substantial portion of the films have mpaa rating info missing.",
            caption = "data were scrapped from imdb.com in 2010")

df = read.csv(text="category,pct
               Other,0.09
               South Asian/South Asian Americans,0.12
               Interngenerational/Generational,0.21
               S Asian/Asian Americans,0.25
               Muslim Observance,0.29
               Africa/Pan Africa/African Americans,0.34
               Gender Equity,0.34
               Disability Advocacy,0.49
               European/European Americans,0.52
               Veteran,0.54
               Pacific Islander/Pacific Islander Americans,0.59
               Non-Traditional Students,0.61
               Religious Equity,0.64
               Caribbean/Caribbean Americans,0.67
               Latino/Latina,0.69
               Middle Eastern Heritages and Traditions,0.73
               Trans-racial Adoptee/Parent,0.76
               LBGTQ/Ally,0.79
               Mixed Race,0.80
               Jewish Heritage/Observance,0.85
               International Students,0.87", stringsAsFactors=FALSE, sep=",",
               header=TRUE)
df$category = reorder(df$category, df$pct)

plt = mk_lollipop(df)
plt("pct", "category", yorder = "descend", font_size = 10, show_pct = T,
    label_decimals = 0)

# compare with horizontal barplot
plt = mk_barploth_resp(df)
plt("pct", "category", yorder = "descend", font_size = 10, show_pct = T,
    label_decimals = 0)

