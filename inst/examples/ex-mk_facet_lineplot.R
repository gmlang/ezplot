library(ezplot)

load("inst/power_n_ssize_gender.rda")
plt = mk_facet_lineplot(power_n_ssize_gender)

# grouping var can be numeric
plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", gpby = "rho")
p = plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", gpby = "rho",
        ylab_rt = "Sample Size", legend_title = bquote(rho))
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
plt("delta", "Power", xvar_top = "csize", yvar_rt = "ssize", gpby = "rho")

