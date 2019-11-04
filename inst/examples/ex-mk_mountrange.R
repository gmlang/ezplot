library(ezplot)

f = mk_mountrange(ggridges::lincoln_weather)
f('Mean Temperature [F]', 'Month', fillby = 'x', cut_tail = 0.005, font_size = 14)
f('Mean Temperature [F]', 'Month', fillby = 'x', cut_tail = 0.01,
  scale_height = 1, hue = 'B', font_size = 14)
f('Mean Temperature [F]', 'Month', fillby = 'x', cut_tail = 0.05,
  scale_height = 0.8, hue = 'A', font_size = 14)

f('Mean Temperature [F]', 'Month', fillby = 'ecdf', cut_tail = 0.005,
  scale_height = 2, font_size = 14)
f('Mean Temperature [F]', 'Month', fillby = 'ecdf', cut_tail = 0.005,
  scale_height = 2, color_direction = -1, font_size = 14)

f('Mean Temperature [F]', 'Month', fillby = 'density', cut_tail = 0.01,
  scale_height = 1, hue = 'D', color_direction = -1, font_size = 14)
f('Mean Temperature [F]', 'Month', fillby = 'ndensity', cut_tail = 0.005,
  scale_height = 2, hue = 'E', color_direction = -1, font_size = 10)
