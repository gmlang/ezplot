library(dplyr)
library(ezplot)

df = data.frame(trt=LETTERS[1:5],
                lt =c(20.5, 40.3, 10.5, 30.2, 50.4),
                rt =c(70.2, 50.9, 30.3, 60.8, 80.1)
                )
f = mk_dumbbell(df)
f("lt", "rt", "trt")
f("lt", "rt", "trt", label_decimals = 2)
f("lt", "rt", "trt", label_decimals = 0)


# # NOT right
# # y must not have repeats
# df2 = data.frame(trt = c(LETTERS[1:5], "D"),
#                  l = c(20, 40, 10, 30, 50, 40),
#                  r = c(70, 50, 30, 60, 80, 70)
#                  )
# f = mk_dumbbell(df2)
# f("l", "r", "trt") %>% add_labs(title = "ggplot2 geom_dumbbell with dot guide")



df = data.frame(country=c("Germany", "France", "Vietnam", "Japan", "Poland",
                          "Lebanon", "Australia", "South\nKorea", "Canada",
                          "Spain", "Italy", "Peru", "U.S.", "UK", "Mexico",
                          "Chile", "China", "India"),
                 age35=c(0.39, 0.42, 0.49, 0.43, 0.51, 0.57,
                           0.60, 0.45, 0.65, 0.57, 0.57, 0.65,
                           0.63, 0.59, 0.67, 0.75, 0.52, 0.48),
                 age18to34=c(0.81, 0.83, 0.86, 0.78, 0.86, 0.90,
                                 0.91, 0.75, 0.93, 0.85, 0.83, 0.91,
                                 0.89, 0.84, 0.90, 0.96, 0.73, 0.69)
                )

f = mk_dumbbell(df)
f("age35", "age18to34", "country", label_decimals = 2)
f("age35", "age18to34", "country", yorder = "ascend", label_decimals = 2)
f("age35", "age18to34", "country", yorder = "descend", label_decimals = 2)
f("age35", "age18to34", "country", show_pct = T, label_decimals = 0)
f("age35", "age18to34", "country", show_pct = T, label_decimals = 0,
  yorder = "ascend")
f("age35", "age18to34", "country", show_pct = T, label_decimals = 0,
  yorder = "descend")


