#' day 1: *Introduction to nonlinear trend detection*
# attach necessary packages
library('dplyr')     # for data wrangling
library('ggplot2')   # for fancy plot
library('ggExtra')   # for marginal histograms
library('mgcv')      # for modeling
library('gratia')    # for plotting models
library('cowplot')   # for multi-panel plots
theme_set(theme_bw())

chick_weight <- janitor::clean_names(ChickWeight) %>%
  as_tibble() %>%
  filter(diet == 1)
chick_weight

ggplot(chick_weight, aes(time, weight, group = chick)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.3)

# part 1:  ----
# linear models fit poorly to exponential growth data
ggplot(chick_weight, aes(time, weight)) +
  geom_line(aes(group = chick), alpha = 0.5) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', formula = y ~ x,
              color = 'darkorange', fill = 'darkorange', alpha = 0.3)

#' many would log-transform the data, but this doesn't fix the issues... 
ggplot(chick_weight, aes(time, log(weight))) +
  geom_line(aes(group = chick), alpha = 0.5) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', formula = y ~ x,
              color = 'darkorange', fill = 'darkorange', alpha = 0.3)

# ...actually, it introduces more:

#' *do not transform response data (nonlinearly)*: Jensen's inequality
lambda <- 10
pois_sample <- rpois(100, lambda = lambda)
mean(pois_sample)

ggplot() +
  geom_histogram(aes(pois_sample), bins = 7,
                 fill = 'grey', color = 'black')

exp(mean(log(pois_sample)))

# always true (unless all values in the sample are the same...)
exp(mean(log(rpois(100, lambda = 10)))) < mean(pois_sample)

ggplot() +
  geom_histogram(aes(log(pois_sample)), bins = 7,
                 fill = 'grey', color = 'black')

# effects are even more marked if SD/mean ratio is large:
pois_sample_2 <- rpois(100, lambda = 3)

ggExtra::ggMarginal(
  ggplot() +
    geom_line(aes(seq(0, ceiling(max(pois_sample_2)), by = 0.01),
                  y = log1p(after_stat(x))),
              color = 'grey') +
    geom_jitter(aes(pois_sample_2, log1p(pois_sample_2)), alpha = 0.3,
                width = 0.25, height = 0.02) +
    geom_vline(aes(color = 'Sample mean', xintercept = mean(pois_sample_2)),
               lwd = 1) +
    geom_hline(aes(color = 'Mean of transformed data',
                   yintercept = mean(log1p(pois_sample_2))), lwd = 1) +
    geom_vline(aes(color = 'Back-transformed mean',
                   xintercept = exp(mean(log1p(pois_sample_2))) - 1), lwd = 1) +
    geom_segment(aes(x = exp(mean(log1p(pois_sample_2))) - 1,
                     xend = mean(pois_sample_2), y = 0.5, yend = 0.5,
                     color = 'Bias'),
                 arrow = grid::arrow(ends = 'both', length = unit(0.1, 'in'))) +
    labs(x = 'Sample data', y = 'Transformed sample data') +
    scale_color_manual(name = 'Variable',
                       values = c('#004488', '#DDAA33', '#BB5566', 'black'),
                       breaks = c('Sample mean', 'Mean of transformed data',
                                  'Back-transformed mean', 'Bias')) +
    theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.2),
          legend.frame = element_rect(fill = 'black')),
  fill = 'grey', type = 'histogram', size = 3,
  xparams = list(bins = 10, fill = '#004488'),
  yparams = list(bins = 10, fill = '#DDAA33'))

#' moving beyond Gaussian models:
#' **fit the model to the data, not the data to the model!**

#'  *choose a family of distributions based on the range of the response*
?stats::family

#' `binomial`: binary (0/1 data; mean is `p = P(success)`)
#' `gaussian`: unbounded data: all real numbers (-Inf, Inf)
#' `Gamma`: `Y > 0`; `Var(Y)` proportional to `E(Y)^2`
#' `poisson`: count data (integers), `Y >= 0`; `Var(Y) = E(Y)`
#' `inverse.gaussian`: `Y > 0; Var(Y) = E(Y)^3 * lambda`; rarely used
#' `quasibinomial`: `binomial` but with over/under-dispersion parameter
#' `quasipoisson`: `poisson` but with over/under-dispersion parameter

?mgcv::family.mgcv
#' `tw`: between Poisson (`p=1`) and Gamma data (`p=2`); `Y >= 0`
#' `Tweedie`: like `tw`, but `p` is specificed (`Y >= 0`)
#' `nb`: n attempts before p successes; overdispersed poisson
#' `negbin`: like `nb`, but scale term is specificed
#' `betar`: ratio data (bounded [0, 1]); can also be used for NDVI

#' `ocat`: ordered categorical data (e.g., small < medium < big)
#' `scat`: scaled t data (unbounded, like Gaussian, but thicker tails)
#' `ziP`: zero-inflated count data (e.g., counts with many zeros)
#' `cox.ph`: cox proportional hazards (survival analysis)
#' `multinom`: unordered categorical data (e.g., colors)

#' `cnorm`, `bcg`, `clog`, `cpois`: censored data

#' *multiple linear predictors (a list of formulae; require lots of data)*
#' *location-scale are for trends in the mean-variance relationship*
#' `mvn`: multivariate normal data (separate variances with a v-cov matrix)
#' `gaulss`: location-scale Gaussian, unbounded data
#' `gammals`: location-scale gamma
#' `ziplss`: location-scale zero-inflated poisson
#' `twlss`: location-scale tweedie
#' `gumbls`: extreme values (maxima, minima)
#' `gevlss`: extreme values (generalization of Gumbel, Fréchet, & Weibull)
#' `shash`: extremely flexible generalization of normal (VERY data-hungry!)

#' *choose a link function based on the support of the distribution*
#' unbounded: identity; `I(-Inf, Inf) = (-Inf, Inf)`
#' `Y >= 0` or `Y > 0`: `log(0, Inf) = (-Inf, Inf)`
#' `0 <= Y <= 1`: `logit(0, 1) = log(odds(0, 1)) = log(0,Inf) = (-Inf,Inf)`
#' there are other options, but these are sufficient (especially with GAMs)

#' *note:* link functions introduce two new terms:
#' - *response scale*: the original response values; e.g., (0, Inf), (0, 1)
#' - *link scale*: the transformed response values; generally (-Inf, Inf)

#' *note:* link function is applied to the *mean*, not to the data directly

#' `E(Y) = mu`
#' `g(mu) = eta = b_0 + b_1 * x_1 + b_2 * x_2`

#' *fitting Generalized Linear Models*

#' *Gaussian models give bad fits*
m_lm <- gam(formula = weight ~ time,
            family = gaussian(link = 'identity'),
            data = chick_weight)
draw(m_lm, parametric = TRUE)

new_d <- tibble(time = seq(0, 21, length.out = 400)) # for predictions

# to plot predictions quickly
plot_preds <- function(.model) {
  fitted_values(object = .model, data = new_d) %>%
    ggplot() +
    geom_ribbon(aes(time, ymin = .lower_ci, ymax = .upper_ci),
                fill = 'darkorange', alpha= 0.3) +
    geom_line(aes(time, y = .fitted), color = 'darkorange') +
    geom_point(aes(time, weight), chick_weight, alpha = 0.3)
}

# does not follow the data well
plot_preds(m_lm)

# gives impossible predictions (negative weight 3-5 days before hatching)
fitted_values(m_lm, data = tibble(time = -5:0))

#' *Gamma GLM fits better*
m_glm <- gam(formula = weight ~ time,
             family = Gamma(link = 'log'),
             data = chick_weight)
draw(m_lm, parametric = TRUE) #' *note:* partial effect is on log scale
plot_preds(m_glm) #' `eta = log(mu)` implies `mu = exp(eta)`

# values are strictly positive, even a million years before hatching!
fitted_values(m_glm, data = tibble(time = -5:0))
fitted_values(m_glm, data = tibble(time = -1e6 * 365.25))

#' *model diagnostics*
#' `plot()` version of `gratia::appraise()`
layout(matrix(1:4, ncol = 2))
gam.check(m_glm)
layout(1)

appraise(m_glm, method = 'simulate', n_simulate = 100, point_alpha = 0.3)

plot_preds(m_glm) # estimated line is too high initially and at the end

#' *check that assumptions are met*
#' 1. Gaussian residuals on the link scale: e ~ N(0, sigma^2)
#' 2. constant scale parameter (constant variance on link scale)
#' 3. linear relationship on the link scale
#' 4. (conditionally) independent observations

#' diagnostic plots (assumptions to check):
#' *qq plot* (1):  points should be near the red line
#' *residuals vs linear predictor* (2-4): points should be a random scatter
#' *histogram* (1): should be close to gaussian
#' *obs vs fitted* (3, 4): points should fall near 1:1 line

# Q: how would you interpret the diagnostic plots?

#' *limitations of GLMs*
# - cannot account for nonlinear trends
# - require polynomials for non-monotonic trends
# - assume relatively rigid relationships


#' **break** --------------------------------------------------------------

# Part 2 ----
#' *fitting Generalized Additive Models*
#' *Gamma GAM fits better*
m_gam <- gam(formula = weight ~ s(time),
             family = Gamma(link = 'log'),
             data = chick_weight,
             method = 'REML') # method for selecting smoothness

draw(m_gam) # sublinear trend (growth slows down: slope decreases)

# GAM has sub-exponential, near-linear growth, especially near the end
plot_grid(plot_preds(m_glm), plot_preds(m_gam))

# again, values are strictly positive because of log link
fitted_values(m_gam, data = tibble(time = -5:0))
fitted_values(m_gam, data = tibble(time = -1e6 * 365.25))

#' *model diagnostics*
appraise(m_gam, method = 'simulate', n_simulate = 100, point_alpha = 0.3)

# Q: how would you interpret the diagnostic plots?

#' *interpreting nonlinear terms*
#' smooth terms are constructed using *splines*: smooth nonlinear functions
#' splines don't have a single slope; the slope changes with the predictor
#' mathematically more complex than `b_1 * x_1`
#' more flexible: learn from the data without forcing relationships
#' still interpretable, unlike "AI" (neural networks, etc.)

#' *selecting model complexity*
d_ouf <- readr::read_csv('data/ouf-sim.csv', show_col_types = FALSE) %>%
  mutate(dec_date = lubridate::decimal_date(date))
d_ouf

ggplot(d_ouf, aes(date, compound_1)) + geom_point(alpha = 0.3)

plot_k <- function(.k) {
  .m <- gam(compound_1 ~ s(dec_date, k = .k, bs = 'cr'),
            Gamma(link = 'log'), d_ouf, method = 'REML')
  
  plot_grid(
    draw(.m, residuals = TRUE, n = 1e3) &
      ggtitle(paste0('k = ', .k, '; EDF = ', round(summary(.m)$edf, 1))),
    draw(basis(.m, "s(dec_date)")))
}

#' `k`: number of knots; results in `k - 1` basis functions
#' `EDF`: effective degrees of freedom; allows to compare to a polynomial
#' `method = 'REML'` somewhat constrains the fit
#' for more info, see:
#' - `doi.org/10.3389/fevo.2018.00149`
#' - `doi.org/10.48550/arXiv.2507.06281`
plot_k(10) #' default `k` is 10 for `s()`

plot_k(3)  #' lowering `k` forces fewer basis functions and a smoother term
plot_k(20) #' increasing `k` allows more basis functions and wiggliness
plot_k(100) #' high `k` allows for strong oscillations
plot_k(200) #' very high `k` allows for small-scale oscillations

# differences are not always substantial
plot_k(20)
plot_k(15)

#' Q: how do you choose `k`?
#' Q: how do you distinguish noise from signal?

#' *modeling seasonal and daily trends*
d_co2 <- tibble(dec_date = as.vector(time(co2)),
                year = floor(dec_date),
                season = dec_date - year,
                co2_ppm = as.vector(co2))
d_co2

ggplot(d_co2, aes(dec_date, co2_ppm)) + geom_point(alpha = 0.3)

m_co2 <- gam(co2_ppm ~ s(year, k = 10) + s(season, bs = 'cc', k = 10),
             data = d_co2, family = Gamma(link = 'log'), method = 'REML',
             knots = list(season = c(0, 1))) # force 0 and 1 to match
draw(m_co2)

#' reducing `k` too much can result in excessively rigid smooths
gam(co2_ppm ~ s(year, k = 10) + s(season, bs = 'cc', k = 10),
    data = d_co2, family = Gamma(link = 'log'), method = 'REML',
    knots = list(season = c(0, 1))) %>%
  draw()

#' increasing `k` too much can prevent the model from converging
gam(co2_ppm ~ s(year, k = 10) + s(season, bs = 'cc', k = 30),
    data = d_co2, family = Gamma(link = 'log'), method = 'REML',
    knots = list(season = c(0, 1))) %>%
  draw()

#' choosing `k` depends on what you consider as signal
draw(m_co2)
gam(co2_ppm ~ s(year, k = 35) + s(season, bs = 'cc', k = 10), # wigglier
    data = d_co2, family = Gamma(link = 'log'), method = 'REML',
    knots = list(season = c(0, 1))) %>%
  draw()
gam(co2_ppm ~ s(year, k = 5) + s(season, bs = 'cc', k = 10), # smoother
    data = d_co2, family = Gamma(link = 'log'), method = 'REML',
    knots = list(season = c(0, 1))) %>%
  draw()

# Q: any major issues in the diagnostics?
appraise(m_co2, method = 'simulate', point_alpha = 0.3)

#' *extra work for those interested*
#' Q: think of some common datasets you use. what distributions would you
#'    use to model the response variable? are there any datasets for which
#'    more than one distribution may be appropriate?
#' Q: look at the wikipedia page for some of those distribution. what are
#'    the relationships between the mean and the variance? are the two
#'    independent, or can you write the variance as a function of the mean?
#' Q: does the relationship make biological sense?
#' Q: how does this relationship affect how you think of the variable?
#' Q: how would this distribution and its mean-variance relationship affect
#'    how you interpret the model you fit?
