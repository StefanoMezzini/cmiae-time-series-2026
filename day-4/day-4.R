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
source('functions/plot_acf.R') #' `ggplot2` version of `stats:::plot.acf()`
source('functions/plot_pacf.R') #' `ggplot2` version of `stats:::plot.pacf()`
theme_set(theme_bw())

d_sim <- read_csv('data/conc-sim.csv', col_types = 'Ddd') %>%
  mutate(dec_date = decimal_date(date),
         year = year(date),
         doy = yday(date))

#' finding rates of change with LMs or GLMs is easy, but the models often
#' don't fit well...
m_glm <- gam(conc ~ dec_date,
             data = d_sim,
             family = Gamma(link = 'log'), # response is strictly positive
             method = 'REML')

# plot is not helpful because CIs widen starting from year 0 CE
draw(m_glm, parametric = TRUE)
draw(m_glm, parametric = TRUE, ci_level = 0.001, ci_alpha = 0) # hide CIs

summary(m_glm) #' slope is change after a year (on the log scale)
coef(m_glm)
exp(coef(m_glm)['dec_date']) # relative change every year

#' you could subtract a value so that `x` is a lower number
#' (generally not ideal because it can cause confusion when predicting)
m_glm_2 <- gam(conc ~ I(dec_date - 2023),
               data = d_sim,
               family = Gamma(link = 'log'), # response is strictly positive
               method = 'REML')
draw(m_glm_2, parametric = TRUE)
summary(m_glm_2) # intercept is shifted, but slope is unchanged
round(coef(m_glm), 5) == round(coef(m_glm_2), 5)

# extract the fitted values (i.e., predictions) and plot them 
fitted_values(m_glm, scale = 'response') %>%
  ggplot() +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'darkorange', alpha = 0.3) +
  geom_point(aes(dec_date, conc), d_sim, alpha = 0.5) +
  geom_line(aes(dec_date, .fitted), color = 'darkorange', lwd = 1) +
  labs(x = 'Year CE', y = 'Concrentration')

#' *note:* change is nonlinear: each year decreases to 78% of the previous year 

# Q: the term is significant, but does it matter? Is this a good estimate?

# Q: what counts as noise and what counts as trend?

# the diagnostics are clearly not great
appraise(m_glm, point_alpha = 0.3)

# model the data using a smooth term
m_smooth <- gam(conc ~ s(dec_date, k = 20),
                data = d_sim,
                family = Gamma(link = 'log'),
                method = 'REML')

#' *note:* `s(dec_date)` is centered at 0
draw(m_smooth, n = 200, residuals = TRUE)

#' `(Intercept)` is now the average response across the smooth term
summary(m_smooth)
exp(coef(m_smooth)['(Intercept)'])
mean(d_sim$conc) #' *note:* `mean(s(dec_date)) != mean(conc)`

# diagnostics are not great but better than the GLM
appraise(m_smooth, point_alpha = 0.3)

#' Q: again, are the oscillations around `s(dec_date)` noise or signal?

# model the data using a very wiggly smooth term
# as the smooth gets more wiggly, errors get smaller
m_wiggly <- gam(conc ~ s(dec_date, k = 150),
                data = d_sim,
                family = Gamma(link = 'log'),
                method = 'REML')
draw(m_wiggly, n = 500, residuals = TRUE)

# right-side plots are better
appraise(m_wiggly, point_alpha = 0.3)

#' Q: is the model overfit? why or why not?

#' *note:* there is still some autocorrelation in the residuals
#'         see the `{mvgam}` package for dealing with autocorrelation in GAMs
#'         `physalia-courses.org/courses-workshops/time-series-in-r/`
#' still much better than `m_smooth` or `m_glm`
plot_grid(plot_acf(m_wiggly) + ggtitle('Wiggly GAM'),
          plot_acf(m_smooth) + ggtitle('Smooth GAM'),
          plot_acf(m_glm) + ggtitle('GLM'),
          nrow = 1)

plot_grid(plot_pacf(m_wiggly) + ggtitle('Wiggly GAM'),
          plot_pacf(m_smooth) + ggtitle('Smooth GAM'),
          plot_pacf(m_glm) + ggtitle('GLM'),
          nrow = 1)

#' the coefficient estimates from the smooth GAM are closes to the model used
#' to generate the data, but whether the model is over-fit depends on how you
#' plan to use it. do you want to simply interpolate across the data points, or
#' are you trying to estimate the true underlying process?

#' **break** --------------------------------------------------------------

#' *an important note on credible intervals* 
#' *pointwise* credible intervals:
#' - at *individual* `years`, `P(true function in CI) = 1 - alpha`
#' - if values across `s(years)` are independent, ~95% of should be in CI
#' - are approximately across-the-function Frequentist confidence intervals
#' *simultaneous* credible intervals:
#' - across *all* `years`, `P(true function in CI) = 1 - alpha`
#' - values across `s(years)` are not assumed to be independent
#' - the CI contains the true function with probability `1 - alpha`

#' *detecting periods of statistically significant change*
#' *note:* derivatives are on the link scale, but the significance applies
#'         to the response scale, too
lvl <- 0.999 # using 99.9% CIs

slopes_smooth <- bind_cols(
  d_sim,
  #' slopes with CIs for across the function: *simultaneous, not pointwise*
  derivatives(m_smooth, data = d_sim, interval = 'simultaneous', level = lvl) %>%
    transmute(est_deriv = .derivative,
              lwr_deriv = .lower_ci,
              upr_deriv = .upper_ci),
  # add smooth estimates on link scale
  smooth_estimates(m_smooth, data = d_sim, select = 's(dec_date)',
                   overall_uncertainty = FALSE, ci_level = lvl) %>%
    add_confint(coverage = lvl) %>%
    transmute(est_smooth = .estimate,
              lwr_smooth = .lower_ci,
              upr_smooth = .upper_ci),
  # add fitted values on link scale
  fitted_values(m_smooth, data = d_sim, scale = 'link', ci_level = lvl) %>%
    transmute(est_link = .fitted,
              lwr_link = .lower_ci,
              upr_link = .upper_ci),
  # add fitted values on response scale
  fitted_values(m_smooth, data = d_sim, scale = 'response', ci_level = lvl) %>%
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
                     parameter == 'smooth' ~ 'Smooth term',
                     parameter == 'link' ~ 'Fitted (link scale)',
                     parameter == 'resp' ~ 'Fitted (response scale)') %>%
           factor(c('Derivative', 'Smooth term', 'Fitted (link scale)',
                    'Fitted (response scale)')))

# plot smooths and derivatives with direction of change
ggplot(slopes_smooth) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  geom_hline(aes(yintercept = yint),
             tibble(yint = 0, parameter = 'Derivative'), lty = 'dashed') +
  geom_ribbon(aes(dec_date, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(dec_date, est, color = direction, group = 1),
            linewidth = 1.5) +
  labs(x = 'Year CE', y = 'Concrentration') +
  scale_color_brewer(name = 'Direction of change', type = 'qual',
                     palette = 6) +
  theme(legend.position = 'top')

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
            linewidth = 1.5) +
  labs(x = 'Year CE', y = 'Concrentration') +
  scale_color_brewer(name = 'Direction of change', type = 'qual',
                     palette = 6) +
  theme(legend.position = 'top')

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
d_dist <- read_csv('data/disturbance-data.csv', col_types = 'dddcd') %>%
  filter(site == 'Site 3')
summary(d_dist)

# quick data viz before fitting a model
density_lab <- expression(Population~density~(km^'-2'))

d_dist %>%
  pivot_longer(c(forest_perc, elevation_m, years)) %>%
  mutate(name = case_when(name == 'forest_perc' ~ 'Forest cover (%)',
                          name == 'elevation_m' ~ 'Elevation (m a.s.l.)',
                          name == 'years' ~ 'Years since disturbance')) %>%
  ggplot(aes(value, animals_per_km2)) +
  facet_wrap(~ name, scales = 'free_x', strip.position = 'bottom',
             ncol = 2) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x), color = 'darkorange',
              fill = 'darkorange') +
  labs(x = NULL, y = density_lab) +
  theme(strip.placement = 'outside', strip.background = element_blank(),
        strip.text = element_text(size = 11))

# fit the model
m_dist <- gam(animals_per_km2 ~ s(forest_perc) + s(elevation_m) + s(years),
              family = tw(link = 'log'), data = d_dist, method = 'REML')
appraise(m_dist, point_alpha = 0.3) # diagnostics are ok
draw(m_dist)

# generate a dataset to predict from with all other variables present
new_d_dist <- data_slice(m_dist, years = evenly(years, n = 500))

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
#' the `add_*` version of the functions bind the data automatically

#' *note:* `smooth_samples()` gives samples for a smooth excluding any
#'          other smooth or parametric term (such as the model intercept),
#'          so the samples are on the link scale and centered at 0.
#'          all other functions are on the response scale
add_smooth_samples(new_d_dist, m_dist, n = 100, select = 's(years)') %>%
  ggplot() + geom_line(aes(years, .value, group = .draw), alpha = 0.1)

#' `fitted_samples()` gives samples predicted on the response scale and
#' including the model intercept
add_fitted_samples(object = new_d_dist, m_dist, n = 100) %>%
  ggplot() + geom_line(aes(years, .fitted, group = .draw), alpha = 0.1)

samples <-
  # simulate samples of the derivatives for the given dataset
  #' `unconditional=TRUE` accounts for uncertainty in smoothness parameter
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
  #' samples of the response: `Var(animals_per_km2) = sigma^2`
  geom_hex(aes(years, posterior), color = '#00000020') +
  #' samples of estimated mean: `Var(mu_hat) = sigma^2 / n`
  geom_line(aes(years, fitted, group = .draw), color = '#FFFFFF80')+
  #' estimated mean: `Var(animals_per_km2) = sigma^2`
  geom_ribbon(aes(years, ymin = .lower_ci, ymax = .upper_ci), years_preds,
              alpha = 0.3, fill = 'darkorange', color = 'darkorange')+
  geom_line(aes(years, .fitted), years_preds, color = 'darkorange',
            linewidth = 1.5) +
  labs(x = 'Years after disturbance', y = density_lab) +
  scale_fill_devon(name = 'Count', reverse = TRUE)

#' *estimating probability of the mean surpassing a threshold*
# simulate many (>= 1e4) draws from the posterior of the estimated mean
means <- add_fitted_samples(new_d_dist, m_dist, n = 1e4,
                            unconditional = TRUE)

# find how many samples exceed the threshold
means_summary <- means %>%
  summarize(years = unique(years),
            forest_perc = unique(forest_perc),
            elevation_m = unique(elevation_m),
            lwr = quantile(.fitted, 0.025), # lower 95% CI
            mu_hat = mean(.fitted),
            upr = quantile(.fitted, 0.975), # upper 95% CI
            p_above_8 = mean(.fitted > 8),
            .by = .row)

# plot the results
ggplot() +
  geom_hline(yintercept = 8, lty = 'dashed', color = 'red3') +
  # simulated draws (spaghetti plot)
  geom_line(aes(years, .fitted, group = .draw), filter(means, .draw <= 2e3),
            alpha = 0.03) +
  # 95% CI from the simulated draws
  geom_ribbon(aes(years, ymin = lwr, ymax = upr), means_summary, lwd = 0.8,
              fill = 'transparent', color = 'black', lty = 'dashed') +
  # estimated mean from the simulated draws
  geom_line(aes(years, mu_hat, color = p_above_8), means_summary,
            linewidth = 1.5) +
  labs(x = 'Years after disturbance', y = density_lab) +
  scale_color_distiller(expression(P(hat(mu)~'>'~8)), palette = 14,
                        direction = 1)

# can also do this with interaction terms
m_dist_ti <-
  gam(animals_per_km2 ~ s(forest_perc) + s(elevation_m) + s(years) +
        ti(forest_perc, years), # interaction between forest_perc and years
      family = tw(link = 'log'), data = d_dist, method = 'REML')

new_d_dist_2 <- data_slice(m_dist_ti,
                           years = evenly(years, 100),
                           forest_perc = evenly(forest_perc, 100))

means_summary_ti <-
  add_fitted_samples(new_d_dist_2, m_dist_ti, n = 500, # for a fast example
                     unconditional = TRUE) %>%
  summarize(years = unique(years),
            forest_perc = unique(forest_perc),
            elevation_m = unique(elevation_m),
            mu_hat = mean(.fitted),
            p_above_8 = mean(.fitted > 8),
            .by = .row)

# population density
ggplot(means_summary_ti) +
  geom_raster(aes(years, forest_perc, fill = mu_hat)) +
  geom_contour(aes(years, forest_perc, z = p_above_8), color = 'black') +
  scale_x_continuous('Years after disturbance', expand = c(0, 0)) +
  scale_y_continuous('Forest cover (%)', expand = c(0, 0)) +
  scale_fill_lipari(name = density_lab)

# P(mu_hat > 8)
ggplot(means_summary_ti) +
  geom_raster(aes(years, forest_perc, fill = p_above_8)) +
  geom_contour(aes(years, forest_perc, z = p_above_8), color = 'black') +
  scale_x_continuous('Years after disturbance', expand = c(0, 0)) +
  scale_y_continuous('Forest cover (%)', expand = c(0, 0)) +
  scale_fill_distiller(expression(P(hat(mu)~'>'~8)), palette = 14,
                       direction = 1)

#' *estimating probability of the data surpassing a threshold*
posterior_samples <- add_posterior_samples(new_d_dist, m_dist, n = 1e4,
                                           unconditional = TRUE)

# hex plot of probability density of the data
ggplot(posterior_samples) +
  geom_hex(aes(years, .response, fill = after_stat(count / sum(count)))) +
  geom_hline(yintercept = 8, lty = 'dashed', color = 'red3') +
  labs(x = 'Years after disturbance', y = density_lab) +
  scale_fill_lapaz(name = 'Density')

# surface plot of the empirical cumulative density function
posterior_samples %>%
  summarize(ecdf_fun = list(ecdf(.response)), .by = years) %>%
  mutate(probs = purrr::map(ecdf_fun, function(.fun) {
    tibble(pop_dens = seq(0, 25, by = 0.01),
           p = .fun(pop_dens))
  })) %>%
  select(! ecdf_fun) %>%
  unnest(probs) %>%
  ggplot() +
  geom_raster(aes(years, pop_dens, fill = p)) +
  geom_hline(yintercept = 8, lty = 'dashed', color = 'red3') +
  labs(x = 'Years after disturbance', y = density_lab) +
  scale_fill_tokyo(name = 'ECDF', reverse = TRUE)

#' *estimating probability of surpassing a threshold (rate of change)*
#' note: other variables are kept constant with `focal = 'years'`
slopes <- derivative_samples(m_dist, focal = 'years', data = new_d_dist,
                             n_sim = 1e4)

ggplot(filter(slopes, .draw <= 100)) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  geom_line(aes(years, .derivative, group = .draw), alpha = 0.1) +
  labs(x = 'Years after disturbance', y = density_lab)

slopes %>%
  summarize(years = unique(years),
            lwr = quantile(.derivative, 0.025), # lower 95% CI
            est_slope = mean(.derivative),
            upr = quantile(.derivative, 0.975), # upper 95% CI
            p_above_0.5 = mean(.derivative > 0.5),
            .by = .row) %>%
  ggplot() +
  geom_hline(yintercept = 0, lty = 'dashed') +
  geom_hline(yintercept = 0.5, lty = 'dashed', color = 'red3') +
  # simulated draws (spaghetti plot)
  geom_line(aes(years, .derivative, group = .draw),
            filter(slopes, .draw <= 2e3), alpha = 0.03) +
  # 95% CI from the simulated draws
  geom_ribbon(aes(years, ymin = lwr, ymax = upr), lwd = 0.8,
              fill = 'transparent', color = 'black', lty = 'dashed') +
  # estimated mean from the simulated draws
  geom_line(aes(years, est_slope, color = p_above_0.5), linewidth = 1.5) +
  labs(x = 'Years after disturbance', y = density_lab) +
  scale_color_distiller('P(Slope > 0.5', palette = 14, direction = 1)

#' *predicting beyond the range of available data*
# we have seen that GAMs are quite efficient at interpolating smoothly 
# because GAMs are so flexible, they can struggle to predict past the data,
# and the uncertainty grows rapidly (and possibly unrealistically)
data_slice(m_dist, years = seq(0, 50, length.out = 400)) %>%
  conditional_values(model = m_dist, condition = 'years', data = .) %>%
  draw() +
  labs(x = 'Years after disturbance', y = density_lab)

#' similar caution should apply with 2D smooths, but the increased
#' flexibility may cause even more issues. However, extrapolations can
#' be quite convenient if the smooth terms are sufficiently smooth, e.g.:
#' - responses to multifarious change: `https://doi.org/10.1111/gcb.17594`
#' - rising temperatures alter mammalian movement: `github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/writing/manuscript.pdf`
#' if you need to forecast past the extent of the data, B-splines may be a
#' good option, but forecasting without deterministic terms is complex if
#' trends are very wiggly. for more info, see:
#' `https://fromthebottomoftheheap.net/2020/06/03/extrapolating-with-gams/`

#' *extra work for those interested*
#' - re-fit `m_wiggly_tw` using an adaptive spline. see `?mgcv::s` and
#'   `?mgcv::smooth.construct.ad.smooth.spec` for more information. how
#'   does the new basis affect the fit? Try setting `k = 20` and comparing
#'   it to the fit using the default basis of `bs = 'tp'`.
#' - create an object like `slopes_smooth` but with `m_wiggly_tw`. how
#'   would you interpret the plot?
#' Q: what are some scenarios in your work where you could implement the
#'    methods shown here for estimating significant rates of change?
#' - repeat the exercises on estimating the probability of exceeding a
#'   threshold, but using less data. how does that affect the results and
#'   your interpretation? how does this relate to statistical power?
m_dist_small <-
  gam(animals_per_km2 ~ s(forest_perc) + s(elevation_m) + s(years),
      family = tw(link = 'log'),
      data = slice_sample(d_dist, prop = 0.1), # 10% sample
      method = 'REML')

#' - repeat the analyses again, but use more data using simulated with
#'   `posterior_samples()` as below, then compare the results again
#' Q: how does increasing the sample size affect the statistical power?
#' Q: how does increasing the sample size affect your perspective of the
#'    typical Frequentist hypothesis-testing approach to statistics? 
d_dist_large <-
  add_posterior_samples(d_dist, m_dist, n = 10) %>%
  select(! animals_per_km2) %>%
  rename(animals_per_km2 = .response)

m_dist_large <-
  gam(animals_per_km2 ~ s(forest_perc) + s(elevation_m) + s(years),
      family = tw(link = 'log'),
      data = d_dist_large, # larger sample
      method = 'REML')

#' For some reading on using rates of change in ecology, see:
#' - critical transitions: `https://doi.org/10.1371/journal.pone.0041010`
#' - responses to multifarious change: `https://doi.org/10.1111/gcb.17594`
#' - lake level & structural changes: `https://doi.org/10.1002/lno.12054`
#' - lake eutrophication: `https://doi.org/10.1111/fwb.14192`
