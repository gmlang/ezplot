library(ezplot)

f = mk_dotplot(days_in_office)
f('days')
f('days', binwidth = 100, dotsize = 1, stackratio = 1.1) %>%
        add_labs(title = 'U.S. Presidents Days in Office')

g = mk_dotplot(mtcars)
g('mpg', fillby = 'cyl', binwidth = .7) %>%
        add_labs(title = 'Miles per gallon by number of cylinders')
