library(dplyr)
library(tidyr)
library(ezplot)

df = ab3 %>% gather(opinion, pct, -Country)
lvls = unique(df$opinion)
plt = mk_likertplot(df)
plt("pct", "Country", fillby = "opinion", fillby_lvls = lvls)
plt("pct", "Country", fillby = "opinion", fillby_lvls = lvls,
    x_as_pct = T, legend_title = "Responses", legend_pos = "top") %>%
    add_labs(xlab = NULL, title = "Confidence estimates for twelve countries' economy")

df = films %>% count(mpaa, made_money) %>% group_by(mpaa) %>%
        mutate(pct = n/sum(n)) %>% ungroup()
plt = mk_likertplot(df)
plt("n", "mpaa", fillby = "made_money", fillby_lvls = c("no", "yes"),
    yorder = "ascend", legend_pos = "left")
plt("pct", "mpaa", fillby = "made_money", fillby_lvls = c("no", "yes"),
    legend_title = "Is Profitable?", x_as_pct = T)

df = data.frame(item = rep(LETTERS[1:5], 4),
                slope = c(rep("North", 10), rep("South", 10)),
                type = rep(c(rep("native", 5), rep("introduced", 5)), 2),
                spp = as.integer(abs(rnorm(20, 5, 2))))
df = df %>% mutate(spp = ifelse(type == "introduced", spp+1, spp),
                   sppInv = ifelse(type == "native", spp, spp*-1))

# plot for only North
plt = mk_likertplot(df %>% filter(slope == "North"))

# works with both negative and positive values
#     sppInv < 0 when type is "introduced", and > 0 when type is "native"
plt("sppInv", "item", fillby = "type", fillby_lvls = c("introduced", "native"),
    legend_pos = "bottom")

# also works with only positive values
#     spp > 0 always
plt("spp", "item", fillby = "type", fillby_lvls = c("introduced", "native"))
