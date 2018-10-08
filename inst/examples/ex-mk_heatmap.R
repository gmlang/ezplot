library(dplyr)
library(tidyr)
library(ezplot)

# --- single heatmap --- #

f = mk_heatmap(films %>% count(made_money, year_cat))
f("year_cat", "made_money", fillby = "n") %>% add_labs(ylab = "Made money?")

df = films %>% group_by(action, year_cat) %>%
        summarise(avg_rating = mean(rating))
f = mk_heatmap(df)

# order colors from lightest to darkest by setting color_direction = 1
f("year_cat", "action", fillby = "avg_rating", legend_title = "Avg Rating",
  font_size = 14, palette = "D", color_direction = 1) %>%
        add_labs(ylab = "Is action film?",
                 title = "Average ratings of action vs. non-action films")

f = mk_heatmap(attacks_all_countries)
f("hour", "wkday", fillby = "n", font_size = 12) %>%
        add_labs(xlab = "nth Hour in a day",
                 title = "Events per weekday & time of day")

# use heatmap to show players' performance by each stats
# by default, the variable Name is read in as a Factor with levels ordered
# alphabetically. Reorder its levels by points
nba$Name = with(nba, reorder(Name, PTS))

# gather all the statistics in one column, and their values in another column.
nba_m = nba %>% gather(stats, val, -Name)
dat = nba_m %>% group_by(stats) %>% mutate(val_scaled = scales::rescale(val))
head(dat)

# make heatmap
f = mk_heatmap(dat)
f("stats", "Name", fillby = "val_scaled", legend_title = "scaled stats") %>%
        rotate_axis_text(text_angle_x = 90, vjust_x = 0.5)


# --- multiple heatmaps --- #

f = mk_heatmap(attacks_by_country)
f("hour", "wkday", fillby ="n", facet_by = "country", facet_ncol = 2,
  legend_title = "Count") %>%
        add_labs(title = "Events per weekday & time of day by country")


