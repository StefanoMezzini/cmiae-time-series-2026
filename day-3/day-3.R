#' day 3: *Applications of GAMs*
# attach necessary packages
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ggplot2')   # for fancy plot
library('mgcv')      # for modeling
library('gratia')    # for plotting models
library('cowplot')   # for multi-panel plots
library('khroma')    # for color palettes
library('lubridate') # for working with dates
theme_set(theme_bw())

#' import the `co2` dataset and change to a format usable by `gam()`
d_co2 <- tibble(dec_date = as.vector(time(co2)),
                year = floor(dec_date),
                season = dec_date - year,
                co2_ppm = as.vector(co2))
d_co2

ggplot(d_co2, aes(dec_date, co2_ppm)) + geom_point(alpha = 0.3)

# fit a GAM as an example
#' `log(mu) = b_0 + f_1(year) + f_2(season) + f_3(year, season)`
#' `mu = exp( b_0 + f_1(year) + f_2(season) + f_3(year, season) )`
#' `= exp(b_0) * exp(f_1(year)) * exp(f_2(season)) * exp(f_3(year, season))`
m_co2 <- gam(co2_ppm ~
               # long-term trend
               s(year, k = 10) +
               # seasonal trend
               s(season, bs = 'cc', k = 10) +
               # change in seasonal trend over the years
               ti(year, season, bs = c('cr', 'cc'), k = 5),
             data = d_co2, family = Gamma(link = 'log'), method = 'REML',
             knots = list(season = c(0, 1))) # force 0 and 1 to match
draw(m_co2)

#' *interpreting model summary*
summary(m_co2)

#' `Family: Gamma`: `y` is assumed to be *conditionally* Gamma-distributed
#' `Formula`: model formula
#' `Parametric coefficients`: table for intercept & slope coefficients
#' `(Intercept)`: `co2_ppm` averaged across all the smooth functions and
#'                setting all other parametric terms to 0
#'  other coefficients: slopes of any parametric terms (i.e., not smooths)
#' `s(year)`: the smooth effect of `year` for the average `s(season)`
#' `ti(year,season)`: change in `s(year)` across `s(season)` and vice-versa
#' 
#' `edf`: estimated degrees of freedom; measures the complexity of smooth
#' `Ref.df`: reference degrees of freedom for F test
#' `F`: F statistic for F test
#' `p-value`: p-value from the F test
#' *note:* (approximate) significance does not imply biological relevance
#' 
#' smooths are centered at 0 to show the deviations from the `(Intercept)`
#' thus, if `year` & `season` are unknown, the best guess is the intercept
#' 
#' `p-values` for smooth terms are approximate and likely too small
#' small changes in `k` can result in very different `p-values`
#' `p-values` decrease quickly with sample size
#' 
#' `R-sq.(adj)` (adjusted R^2): only useful for Gaussian models
#' `Deviance explained`: use as `R-sq.` for non-Gaussian models
#' `Scale est.`: estimated scale parameter
#' `n`: sample size

#' *predicting with GLMs & GAMs*
new_d <- tibble(year = seq(1950, 2000, length.out = 5),
                season = 0) # January 1st

# predictions on the link (log) scale
predict(m_co2, newdata = new_d, type = 'link')

# predictions on the response scale
predict(m_co2, newdata = new_d, type = 'response')
predict(m_co2, newdata = new_d, type = 'link') %>% exp()

# also calculate standard error: returns a list
predict(m_co2, newdata = new_d, type = 'link', se.fit = TRUE)

#' only predict for specific terms by specifying `terms` argument
#' e.g., `terms = 's(year)'`:
#' - excludes `(Intercept)`
#' - sets `s(season) = ti(year,season) = 0`
#' - returns the relative change over `year`s, centered at log(1) = 0
predict(m_co2, newdata = new_d, type = 'link', terms = 's(year)')
predict(m_co2, newdata = new_d, type = 'response', terms = 's(year)')

#' exclude specific terms by specifying `exclude` argument
#' e.g., `exclude = 's(year)'`:
#' - sets `s(year) = 0`, so `exp(s(year)) = exp(0) = 1`
#' - includes `(Intercept)`, `s(season)`, and `ti(year,season)`
predict(m_co2, newdata = new_d, type = 'link',
        exclude = c('(Intercept)', 's(season)', 'ti(year,season)'))

#' **break** --------------------------------------------------------------

#' *plotting model terms*
# relative change over the years
tibble(year = seq(1959, 1997, length.out = 400),
       season = 0) %>% # value is irrelevant because the smooth is excluded
  mutate(rel_change = predict(m_co2, newdata = ., type = 'response',
                              terms = 's(year)')) %>%
  ggplot() +
  geom_line(aes(year, rel_change), linewidth = 1) +
  geom_hline(yintercept = exp(0), lty = 'dashed') + # reference line
  labs(x = 'Year CE',
       y = expression(Relative~change~'in'~CO[2]~concentration~(ppm)))

# change over the years, ignoring seasonal trends
tibble(year = seq(1959, 1997, length.out = 400),
       season = 0) %>% # value is irrelevant because the smooth is excluded
  mutate(mu_hat = predict(m_co2, newdata = ., type = 'response',
                          terms = c('s(year)', '(Intercept)'))) %>%
  ggplot() +
  geom_line(aes(year, mu_hat), linewidth = 1) +
  geom_hline(yintercept = exp(coef(m_co2)['(Intercept)']), lty = 'dashed')+
  labs(x = 'Year CE',
       y = expression(CO[2]~concentration~(ppm)))

# change over the years, including seasonal trends
preds_full <- tibble(year = seq(1959, 1997 - 0.001, by = 0.001),
                     season = year - floor(year)) %>%
  mutate(mu_hat = predict(m_co2, newdata = ., type = 'response'))

# line plot
ggplot(preds_full) +
  geom_line(aes(year, mu_hat), linewidth = 1) +
  geom_hline(yintercept = exp(coef(m_co2)['(Intercept)']), lty = 'dashed')+
  labs(x = 'Year CE', y = expression(CO[2]~concentration~(ppm)))

# group by season
ggplot(preds_full) +
  geom_line(aes(year, mu_hat, color = season * 365, group = season),
            linewidth = 1) +
  geom_hline(yintercept = exp(coef(m_co2)['(Intercept)']), lty = 'dashed')+
  labs(x = 'Year CE', y = expression(CO[2]~concentration~(ppm))) +
  scale_color_gradientn(name = 'Day of year',
                        colors = c(color('batlow')(10),
                                   color('batlow', reverse = TRUE)(10)))

# surface plot by year and day of year
ggplot(preds_full) +
  geom_raster(aes(season * 365, floor(year), fill = mu_hat)) +
  scale_x_continuous('Day of year', expand = c(0, 0)) +
  scale_y_continuous('Year CE', expand = c(0, 0)) +
  scale_fill_lipari(name = expression(Mean~CO[2]~(ppm)))

#' add confidence intervals using `gratia::fitted_values()`
new_d_year <- tibble(year = seq(1959, 1997, by = 0.001), season = 0)
fitted_values(m_co2, data = new_d_year, ci_level = 0.999)

# add confidence intervals manually
dof <- m_co2$df.residual
preds_year_ci <-
  bind_cols(new_d_year,
            predict(m_co2, new_d_year, type = 'link', se.fit = TRUE,
                    exclude = c('s(season)', 'ti(year,season)')) %>%
              as.data.frame()) %>%
  mutate(mu_hat = exp(fit),
         lower_95_ci = exp(fit + qt(0.0005, df = dof) * se.fit), # 99.9% CI
         upper_95_ci = exp(fit + qt(0.9995, df = dof) * se.fit))
preds_year_ci

p_year <-
  ggplot(preds_year_ci) +
  geom_ribbon(aes(year, ymin = lower_95_ci, ymax = upper_95_ci),
              alpha = 0.3) +
  geom_line(aes(year, mu_hat)) +
  labs(x = 'Year CE', y = expression(CO[2]~concentration~(ppm)))
p_year

#' *interpreting credible intervals produced by mgcv*

#' CIs have close to nominal Frequentist coverage properties:
#' - on average, the Bayesian credible intervals produced by `mgcv` contain
#'   `(1 - alpha) * 100%` of the true function, i.e., each value of the
#'   predictor variable has a "pointwise" Frequentist confidence interval
#' - this breaks down with smooth terms that are close to a straight line
#'   and estimated as a straight line, but including the intercept fixes
#'   the issue
#'   see `https://doi.org/10.1111/j.1467-9469.2011.00760.x` for more info
set.seed(10)
d_ci <- as_tibble(gamSim(n = 400))
d_ci

m_ci <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = d_ci, method = 'REML')
#' on average, the CIs should contain `(1-alpha)*100%` of the true function
draw(m_ci)

#' on average, CIs for `s(x3)` will contain less than `(1 - alpha) * 100%`
#' of the true function because their width is zero near `x3 = 0.5`
draw(m_ci, overall_uncertainty = FALSE)

#' *simultaneous* intervals (i.e., calculated for all points at once;
#' covered more later) contain the *entire* true function with probability
#' `(1 - alpha)`

#' *creating publication-level figures*
p_doy <-
  tibble(year = 1959, # excluded below
         season = seq(0, 1, length.out = 400),
         doy = season * 365) %>%
  bind_cols(.,
            predict(m_co2, ., type = 'link', se.fit = TRUE,
                    exclude = c('s(year)', 'ti(year,season)')) %>%
              as.data.frame()) %>%
  mutate(mu_hat = exp(fit),
         lower_95_ci = exp(fit + qt(0.0005, df = dof) * se.fit), # 99.9% CI
         upper_95_ci = exp(fit + qt(0.9995, df = dof) * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(doy, ymin = lower_95_ci, ymax = upper_95_ci),
              alpha = 0.3) +
  geom_line(aes(doy, mu_hat)) +
  #' avoid spaces on the edges using `expand`
  scale_x_continuous('Day of year', expand = c(0, 0)) +
  ylab(expression(CO[2]~concentration~(ppm)))
p_doy

p_ti <-
  expand_grid(year = seq(1959, 1997, length.out = 400),
              season = seq(0, 1, length.out = 400)) %>%
  mutate(doy = season * 365) %>%
  mutate(rel_change = predict(m_co2, ., type = 'response', se.fit = FALSE,
                              terms = 'ti(year,season)') %>%
           as.numeric(),
         percent_change = (rel_change - 1) * 100) %>% #' `predict()` returns array: make vector
  ggplot() +
  geom_raster(aes(doy, year, fill = percent_change)) +
  scale_x_continuous('Day of year', expand = c(0, 0)) +
  scale_y_continuous('Year CE', expand = c(0, 0)) +
  scale_fill_vik(name = expression(atop(Relative~change~'in'~CO[2],
                                        concentration~('%'))),
                 midpoint = 0, limits = c(-0.025, 0.025)) +
  theme(legend.position = 'top', legend.key.width = rel(2))
p_ti

# add all plots together
plot_grid(plot_grid(p_year, p_doy, labels = 'AUTO', nrow = 1),
          p_ti, labels = c('', 'C'), ncol = 1)

#' **break**

#' *deciding sampling frequency before data collection*

#' There is no clear rule for deciding sampling frequency exactly, but you
#' should aim to have (at least) a few samples for each period of interest,
#' if possible. For example, if you want to estimate monthly levels of a
#' certain compound, a measurement every month may be ok, but multiple
#' measurements per month is often better.
d_conc <- readr::read_csv('data/conc-sim.csv', col_types = 'Ddd') %>%
  mutate(month = paste(month.name[month(date)], year(date)),
         week = floor(as.numeric(date) / 7)) %>% # n 7-day periods since origin
  mutate(monthly_mean = mean(conc), .by = month)

## daily measurements
# monthly time series and means
ggplot(d_conc, aes(date, conc)) +
  facet_wrap(~ month, scales = 'free') +
  geom_line(aes(date, monthly_mean), color = 'darkorange') +
  geom_line() +
  geom_point()

# full time series with monthly mean from full dataset
ggplot(d_conc, aes(date, conc)) +
  geom_line() +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = monthly_mean), color = 'darkorange', lwd = 1)

## weekly measurements
# monthly time series and sample means
# differences are present but not always alarming
d_conc %>%
  slice(1, .by = week) %>%
  mutate(sample_mean = mean(conc), .by = month) %>%
  ggplot(aes(date, conc)) +
  facet_wrap(~ month, scales = 'free') +
  geom_line(aes(date, monthly_mean), color = 'darkorange') +
  geom_line(aes(date, sample_mean), color = 'dodgerblue4') +
  geom_line() +
  geom_point()+
  geom_point(data = d_conc, alpha = 0.1)

# thinned time series with monthly mean from full dataset
d_conc %>%
  slice(1, .by = week) %>%
  ggplot(aes(date, conc)) +
  geom_line(aes(y = monthly_mean), color = 'darkorange', lwd = 1) +
  geom_line(data = d_conc) +
  geom_point() +
  geom_point(data = d_conc, alpha = 0.1)

## monthly measurements
# differences can be quite prominent
d_conc %>%
  slice(15, .by = month) %>%
  ggplot(aes(date, conc)) +
  facet_wrap(~ month, scales = 'free') +
  geom_point(aes(date, monthly_mean), color = 'darkorange') +
  geom_point(aes(date, conc), color = 'dodgerblue4') +
  geom_point(data = d_conc, alpha = 0.1)

# thinned time series
d_conc %>%
  slice(15, .by = month) %>%
  ggplot(aes(date, conc)) +
  geom_line(aes(y = monthly_mean), d_conc, color = 'darkorange', lwd = 1) +
  geom_line() +
  geom_point() +
  geom_point(data = d_conc, alpha = 0.1)

#' multiple measurements per period of interest allows one to account for
#' the noise in the data and separate it from the signal. one sample per
#' period prevents us from being able to distinguish the two.

#' *extra work for those interested*
#' - now that you have some experience with GAMs, try fitting some to some
#'   of your datasets using the families you explored in the questions
#'   from day 2. try changing the values of `k`, and maybe exploring the
#'   different types of bases -- see `?mgcv::smooth.terms` for more info.
#' Q: what happens if you set `k` above the number of unique values of the
#'    predictor variable? why do you think this happens?
#' - try creating some figures for the models. can you think of scenarios
#'   where you may want to exclude terms from the predictions? when could
#'   you predict for a single smooth and even exclude the intercept term?
#' Q: since GAM's smooth terms don't have a single coefficient estimate,
#'    how would you determine if changes are significant over a predictor
#'    variable (e.g., time, level of contamination, population density)?

#' *if you want to learn more*
#' In the context of animal tracking, the manuscripts below address
#' some issues that may be of interest. Although these references are
#' specific to animal tracking, the ideas of sampling intensity apply to
#' time series in general. In this case, speed is the trend we want to
#' estimate, and directional persistence is the amount of time for which an
#' animal will move at approximately with the same speed and direction.
#' Generalizing this idea, a minimum of 3 samples per period (ideally more)
#' will give an appreciable estimates for that period (e.g., three samples
#' per month for a monthly average). In other words, the sampling interval
#' should be no larger than a third of the period of interest (e.g., every
#' 10 days for a 30-day average).
#' 
#' References for further reading:
#' - coarse sampling hides information and can produce incorrect results:
#'   `https://doi.org/10.1126/science.abg1780`
#' - how fine should sampling be to estimate speed accurately?
#'   `https://doi.org/10.1186/s40462-019-0177-1`
#' - what happens if you try forcing speed estimation when the signal is
#'   not in the data?
#'   `https://doi.org/10.1101/2025.07.17.665364`
