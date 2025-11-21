#' day 4: *Interpreting nonlinear models*
#' attach necessary packages
library('readr')     # for importing tidy csvs
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ggplot2')   # for fancy plot
library('mgcv')      # for modeling
library('gratia')    # for plotting models
library('cowplot')   # for multi-panel plots
library('khroma')    # for color palettes
library('lubridate') # for working with dates
theme_set(theme_bw())

d_ouf <- read_csv('data/ouf-sim.csv', col_types = 'Ddd') %>%
  mutate(dec_date = decimal_date(date),
         year = year(date),
         doy = yday(date))

#' finding rates of change with LMs or GLMs is easy, but the models often
#' don't fit well...
m_glm <- gam(compound_1 ~ dec_date,
             data = d_ouf,
             family = Gamma(link = 'log'), # response is strictly positive
             method = 'REML')

# plot is not helpful because CIs widen starting from year 0 CE
draw(m_glm, parametric = TRUE)
draw(m_glm, parametric = TRUE, ci_level = 0.001, ci_alpha = 0) # hide CIs

#' slope is -0.345 on the log scale
#' drops to `exp(-0.345) = 0.708 = 70.8%` of the previous year's value
summary(m_glm)

# Q: the term is significant, but does it matter? Is this a good estimate?

# Q: what counts as noise and what counts as trend?
fitted_values(m_glm, scale = 'response') %>%
  ggplot() +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'darkorange', alpha = 0.3) +
  geom_point(aes(dec_date, compound_1), d_ouf, alpha = 0.5) +
  geom_line(aes(dec_date, .fitted), color = 'darkorange', lwd = 1) +
  labs(x = 'Year CE', y = 'Compound 1')

# diagnostics are clearly not great
appraise(m_glm, point_alpha = 0.3)

# model the data using a smooth term
m_smooth <- gam(compound_1 ~ s(dec_date, k = 20),
                data = d_ouf,
                family = Gamma(link = 'log'),
                method = 'REML')
draw(m_smooth, n = 200, residuals = TRUE)

# diagnostics are not great but better than the GLM
appraise(m_smooth, point_alpha = 0.3)

#' Q: again, are the oscillations around the smooth noise or signal?

# model the data using a very wiggly smooth term
m_wiggly <- gam(compound_1 ~ s(dec_date, k = 200),
                data = d_ouf,
                family = Gamma(link = 'log'),
                method = 'REML')
draw(m_wiggly, n = 500, residuals = TRUE)

# diagnostics are better, but mean-variance relationship is wrong
# tails are too long or middle quantiles are too concentrated
appraise(m_wiggly, point_alpha = 0.3)

# re-fit with Tweedie family
m_wiggly_tw <- gam(compound_1 ~ s(dec_date, k = 400),
                   data = d_ouf,
                   family = tw(link = 'log'),
                   method = 'REML')

# diagnostics look reasonable now
#' specifying `n_simulate` because `method = 'uniform'` is not available
appraise(m_wiggly_tw, point_alpha = 0.3, n_simulate = 10)

#' estimated mean is essentially identical to the Gamma model...
draw(m_wiggly_tw, n = 500, residuals = TRUE)

#' ... but the mean-variance relationship is different
#' Gamma variance is `phi * mu^2`
#' Tweedie variance is `phi * mu^p` for `p` in `(1, 2)`
#' the model has `p = 1.01`, so the variance is approximately `phi * mu`

#' Q: is the model overfit?

#' *note:* there is still some autocorrelation in the residuals
acf(resid(m_wiggly_tw), plot = FALSE) %>%
  with(data.frame(lag = lag, acf = acf)) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(aes(x = lag, y = 0, xend = lag, yend = acf)) +
  geom_hline(yintercept = qnorm(0.025)/sqrt(nrow(d_ouf)),
             linetype = 3, color = 'darkblue') +
  geom_hline(yintercept = qnorm(0.975)/sqrt(nrow(d_ouf)),
             linetype = 3, color = 'darkblue')

#' *detecting periods of statistically significant change*
#' *note:* derivatives are on the link scale, but the significance applies
#'         to the response scale, too
slopes_smooth <- bind_cols(
  d_ouf,
  # slopes with CIs for across the function (not pointwise)
  derivatives(m_smooth, data = d_ouf, interval = 'simultaneous',
              level = 0.99) %>%
    transmute(est_deriv = .derivative,
              lwr_deriv = .lower_ci,
              upr_deriv = .upper_ci),
  # add fitted values on link scale
  fitted_values(m_smooth, data = d_ouf, scale = 'link') %>%
    transmute(est_link = .fitted,
              lwr_link = .lower_ci,
              upr_link = .upper_ci),
  # add fitted values on response scale
  fitted_values(m_smooth, data = d_ouf, scale = 'response') %>%
    transmute(est_resp = .fitted,
              lwr_resp = .lower_ci,
              upr_resp = .upper_ci)) %>%
  # add colors for slopes and check if change is significant
  mutate(direction = case_when(est_deriv > 0 ~ 'Increasing',
                               est_deriv < 0 ~ 'Decreasing',
                               est_deriv == 0 ~ 'No change'),
         significant = lwr_deriv > 0 | upr_deriv < 0,
         group = c(TRUE, c(significant != lag(significant))[-1]) %>%
           cumsum()) %>%
  # pivot to long format
  pivot_longer(cols = est_deriv:upr_resp, values_to = 'value',
               names_to = c('estimate_type', 'parameter'),
               names_sep = '_') %>%
  pivot_wider(values_from = value, names_from = estimate_type) %>%
  mutate(parameter =
           case_when(parameter == 'deriv' ~ 'Derivative',
                     parameter == 'link' ~ 'Fitted (link scale)',
                     parameter == 'resp' ~ 'Fitted (response scale)'))

# plot smooths and derivatives with direction of change
ggplot(slopes_smooth) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  geom_hline(aes(yintercept = yint),
             tibble(yint = 0, parameter = 'Derivative'), lty = 'dashed') +
  geom_ribbon(aes(dec_date, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(dec_date, est, color = direction, group = 1),
            linewidth = 1) +
  labs(x = 'Year CE', y = 'Compound 1') +
  scale_color_brewer(name = 'Direction of change', type = 'qual', palette = 6)

#' plot smooths and derivatives with direction of *significant* change
ggplot(slopes_smooth) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  geom_hline(aes(yintercept = yint),
             tibble(yint = 0, parameter = 'Derivative'), lty = 'dashed') +
  geom_ribbon(aes(dec_date, ymin = lwr, ymax = upr),
              alpha = 0.2) +
  geom_line(aes(dec_date, est), alpha = 1) +
  geom_line(aes(dec_date, est, color = direction, group = group),
            data = filter(slopes_smooth, significant),
            linewidth = 1) +
  labs(x = 'Year CE', y = 'Compound 1') +
  scale_color_brewer(name = 'Direction of change', type = 'qual',
                     palette = 6)

#' *choosing how many years to monitor*
#' - >= 3 samples per period (see `day3.R`)
#' - sample length should depend on the decay of the signal
#' - duration should be longer than the expected time before change,
#'   especially if you are interested in more than just mean conditions,
#'   e.g.: changes in variance; not covered here
#' 
#' example: we want to estimate the population density of a species
#' following a disturbance. we account for confounding effects: forest
#' cover and elevation.
d_dist <- read_csv('data/disturbance-data.csv', col_types = 'dddfd') %>%
  filter(site == 'Site 3')
summary(d_dist)

# quick data viz before fitting a model
d_dist %>%
  pivot_longer(c(perc_forest, elevation_m, years)) %>%
  mutate(name = case_when(name == 'perc_forest' ~ 'Forest cover (%)',
                          name == 'elevation_m' ~ 'Elevation (m a.s.l.)',
                          name == 'years' ~ 'Years since disturbance')) %>%
  ggplot(aes(value, animals_per_km2)) +
  facet_wrap(~ name, scales = 'free_x', strip.position = 'bottom',
             ncol = 2) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x), color = 'darkorange',
              fill = 'darkorange') +
  labs(x = NULL, y = expression(Population~density~(km^'-2'))) +
  theme(strip.placement = 'outside', strip.background = element_blank(),
        strip.text = element_text(size = 11))

# fit the model
m_dist <- gam(animals_per_km2 ~ s(perc_forest) + s(elevation_m) + s(years),
              family = tw(link = 'log'), data = d_dist, method = 'REML')
appraise(m_dist, point_alpha = 0.3) # diagnostics are ok
draw(m_dist)

# generate a dataset to predict from with all other variables present
new_d_dist <- data_slice(m_dist, years = evenly(x = years, n = 500))

# get predictions (note: confounding variables are fixed)
years_preds <- fitted_values(object = m_dist, data = new_d_dist,
                             scale = 'response', ci_level = 0.99)
years_preds

# Q: what would your conclusion have been if you had only sampled for...
# ... 2 years?
years_preds %>%
  filter(years <= 2) %>%
  ggplot() +
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3)+
  geom_line(aes(years, .fitted))

# ... 5 years?
years_preds %>%
  filter(years <= 5) %>%
  ggplot() +
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3)+
  geom_line(aes(years, .fitted))

# ... 10 years?
years_preds %>%
  filter(years <= 10) %>%
  ggplot() +
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3)+
  geom_line(aes(years, .fitted))

# ... 15 years?
years_preds %>%
  filter(years <= 15) %>%
  ggplot() +
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3)+
  geom_line(aes(years, .fitted))

# ... 25 years?
years_preds %>%
  ggplot() +
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3)+
  geom_line(aes(years, .fitted))

#' Q: what is a fundamental problem with the sampling in this example?

#' *sampling from the model*
#' four different types of sampling functions from the `gratia` package:
#' `smooth_samples()`    includes uncertainty in each smooth individually
#' `fitted_samples()`    includes uncertainty in the estimated coefficients
#' `predicted_samples()` includes uncertainty in sampling of the response
#' `posterior_samples()` includes uncertainty in both the coefs & response

#' *note:* `smooth_samples()` gives samples for a smooth excluding any
#'          other smooth or parametric term (such as the model intercept),
#'          so the samples are on the link scale and centered at 0.
#'          all other functions are on the response scale
add_smooth_samples(new_d_dist, m_dist, n = 100, select = 's(years)') %>%
  ggplot() + geom_line(aes(years, .value, group = .draw), alpha = 0.1)

#' the `add_*` version of the functions bind the data automatically
add_fitted_samples(object = new_d_dist, m_dist, n = 100) %>%
  ggplot() + geom_line(aes(years, .fitted, group = .draw), alpha = 0.1)

samples <-
  derivative_samples(m_dist, focal = 'years', data = new_d_dist,
                     n_sim = 100, unconditional = TRUE) %>%
  rename(derivative = .derivative) %>%
  # add samples from the posterior of the estimated mean
  left_join(fitted_samples(m_dist, new_d_dist, n = 100, unconditional = TRUE) %>%
              rename(fitted = .fitted),
            by = c('.row', '.draw')) %>%
  # add samples from the estimated posterior of the response
  left_join(posterior_samples(m_dist, new_d_dist, n = 100, unconditional = TRUE) %>%
              rename(posterior = .response),
            by = c('.row', '.draw')) %>%
  # sample the response without accounting for model uncertainty
  left_join(predicted_samples(m_dist, new_d_dist, n = 100) %>%
              rename(predicted = .response),
            by = c('.row', '.draw'))

#' *note*: the CIs are pointwise, not simultaneous
ggplot(samples) +
  #' samples of estimated mean: `Var(mu_hat) = sigma^2 / n`
  geom_hex(aes(years, posterior), color = '#00000020') +
  #' samples of the response: `Var(animals_per_km2) = sigma^2`
  geom_line(aes(years, fitted, group = .draw), color = '#FFFFFF80')+
  #' estimated mean: `Var(animals_per_km2) = sigma^2`
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), years_preds,
              alpha = 0.3, fill = 'darkorange', color = 'darkorange')+
  geom_line(aes(years, .fitted), years_preds, color = 'darkorange',
            linewidth = 1) +
  labs(x = 'Years after disturbance',
       y = expression(Population~density~(km^'-2'))) +
  scale_fill_devon(name = 'Count', reverse = TRUE)

#' *estimating probability of surpassing a threshold (mean)*

#' *estimating probability of surpassing a threshold (rate of change)*
#' note: other variables are kept constant
#' number of rows is `nrow(newd_d_dist) * n_sim = 500 * 1e4`
derivative_samples(m_dist, focal = 'years', data = new_d_dist, n_sim = 100) %>%
  ggplot() +
  geom_line(aes(years, .derivative, group = .draw), alpha = 0.1)

#' *repreat P(Y > threshold) with less data?*

#' *predicting beyond the range of available data (univariate)*


#' *predicting beyond the range of available data (bivariate)*

