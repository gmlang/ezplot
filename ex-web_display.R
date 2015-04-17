library(ezplot)

# make some data
df = read.table(header=TRUE, text='
student grade
Joe 90
Mary 75
Alex 50')

# draw a barplot
barplt = mk_barplot(df)
p = barplt("student", "grade", "student", xlab="Student", ylab="Grade") 
print(p)

# make the barplot web-ready
web_display(p) 
web_display(p, legend_pos="none")
web_display(p, legend_pos="top")
web_display(p, legend_pos="bottom")
