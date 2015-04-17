# make some data
df = read.table(header=TRUE, text='
student grade
Joe 90
Mary 75
Alex 50')
df$pct = df$grade / sum(df$grade)

df2 = read.table(header=TRUE, text='
group level val
A      small 1.8
A      medium 2.2
A      large 1.5
B      small 2.0
B      medium 2.6
B      large 1.0
C      small 2.5
C      medium 1.3
C      large 2.9')

# calculate the percentage of the levels within each group
library(tidyr)
library(dplyr)

pct = df2 %>% spread(level, val)
temp = pct[, 2:4]
pct = cbind(group=pct[, 1], temp / apply(temp, 1, sum))
pct = pct %>% gather(level, pct, -group)
pct$level = factor(pct$level, levels=c("small", "medium", "large"))

# make barplots
library(ezplot)

# regular barplots
barplt = mk_barplot(df)
barplt("student", "grade", "student") 
barplt("student", "grade", "student", legend=F) 
barplt("student", "pct", "student", legend=F, ypct=T) 
barplt("student", "pct", "student", legend=F)

barplt = mk_barplot(df2)
barplt("group", "val", "level") 

# stacked barplot to display the percentages
barplt = mk_barplot(pct)
p = barplt("group", "pct", "level", ypct=T)
print(p)

# use color-blind friendly palettes
cbPalette = palette("cb_gray")
cbbPalette = palette("cb_black")
p + ggplot2::scale_fill_manual(values=cbPalette)
p + ggplot2::scale_fill_manual(values=cbbPalette)

# use customized palettes
red = palette("red")
purple = palette("purple")
green = palette("green")
p + ggplot2::scale_fill_manual(values=c(red, purple, green))

# use RColorBrewer palettes
p + ggplot2::scale_fill_brewer()
p + ggplot2::scale_fill_brewer(palette="Set1")
p + ggplot2::scale_fill_brewer(palette="Spectral")


# adjust sizes of ylab, xlabs, ytick and xtick labels
p = barplt("group", "pct", "level", ypct=T, xlab="Group", ylab="Percent")
print(p)
