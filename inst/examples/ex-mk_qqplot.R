library(ezplot)

set.seed(0)
smp = data.frame(norm = rnorm(100))
plt = mk_qqplot(smp)
plt("norm") %>% square_fig() # all points should randomly scatter around y = 0
plt("norm", ci_band_type = "ts", font_size = 10) %>% square_fig()
plt("norm", detrend = FALSE) %>% square_fig() # all points should fall inside of CI band
plt("norm", detrend = FALSE, ci_band_type = "ts") %>% square_fig()

set.seed(23)
smp = data.frame(norm = rnorm(100, mean = 50, sd = 2))
plt = mk_qqplot(smp)
plt("norm") %>% square_fig() # check if points randomly scatter around y = 0, they should.
plt("norm", detrend = FALSE) %>% square_fig() # check if most points fall inside of CI band, they should.

plt = mk_qqplot(airquality)
plt("Ozone", dist = "exp", dparams = list(rate = 0.022)) %>% square_fig() %>%
    add_labs(title = "The mean ozone levels from the airquality dataset is approximately exponential",
             caption = "Theoretical Distribution: Exponential with rate 0.022")
plt("Ozone", dist = "exp", dparams = list(rate = 0.022), detrend = F) %>%
        square_fig()


set.seed(2323)
log_bo = sample(log(films$boxoffice), 100)
plt = mk_qqplot(data.frame(log_bo))
plt("log_bo") %>% square_fig() # if points form a pattern, then not normal. If some points fall outside of CI band, then not normal
plt("log_bo", detrend = F) %>% square_fig() # if not all points are inside of the blue CI band, the distribution is not normal
