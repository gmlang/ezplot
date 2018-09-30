# make some data
df = read.table(header=TRUE, text='
student grade
Joe 90
Mary 75
Alex 50', stringsAsFactors=FALSE)

# draw a barplot
plt = mk_barplot_resp(df)
p = plt("student", "grade")
print(p)

# rotate the x-axis tick text
rotate_axis_text(p, text_angle_x = 90, vjust_x = 0.4)

# draw a horizontal barplot
plt = mk_barploth_resp(df)
p = plt("grade", "student")
print(p)

# rotate the y-axis tick text
rotate_axis_text(p, text_angle_y = 90, hjust_y = 0.5)
