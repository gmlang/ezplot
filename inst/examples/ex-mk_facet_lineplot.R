library(ezplot)

plt = mk_facet_lineplot(power_n_ssize_gender)

# grouping var can be numeric
plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", colorby = "rho")
p = plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", colorby = "rho",
        ylab_rt = "Sample Size", legend_title = bquote(rho), legend_pos = "left")
print(p)
cap = "delta: outcome difference between male and female;
         rho: correlation coefficient within animal."
add_labs(p, xlab = bquote(delta),
         title = "Powers & Sample sizes when Testing Main Effect: Gender, Male vs. Female",
         subtitle = "Number of cells per animal",
         caption = cap)

# grouping var can be character/factor
df = power_n_ssize_gender
df$rho = as.character(df$rho)
plt = mk_facet_lineplot(df)
plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", colorby = "rho",
    legend_title = bquote(rho), legend_pos = "bottom")

