#' day 1: *Review and intro to linear trend detection*
#' 
#' before we start...
#' run `necessary-packages.R` to install all the packages we'll use
#' 
#' Q: how many have *not* used `R` much before?
#' Q: how many have *not* used functional programming (e.g., `apply()`)?
#' Q: how many are *not* used to `tidyverse` syntax (`%>%`, etc.)?
#' Q: how many do *not* have recent experience with statistics?

# attach necessary packages
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('lubridate') # for working with dates
library('ggplot2')   # for fancy plot
library('mgcv')      # for modeling
library('gratia')    # for plotting models
library('cowplot')   # for multi-panel plots
theme_set(theme_bw())

# part 1: review of core statistical concepts ----
#' *populations*
#' imagine all individuals of species "X" within a given period of time.
#' let the population of all masses have a mean `mu` and variance `sigma`:
mu <- 14.5 # true mean
sigma <- 2.2 # true standard deviation
densities <- tibble(mass_kg = seq(0, 30, length.out = 1e3),
                    density = dnorm(mass_kg, mean = mu, sd = sigma))

# plot the (unknown?) population distribution
ggplot() +
  geom_area(mapping = aes(mass_kg, density), data = densities,
            fill = 'grey', color = 'black') +
  labs(x = 'Mass (kg)', y = 'Density')

# Q: in the "real world" can we ever know the true population statistics?

# Q: what are some populations that are not a set of individuals?

# Q: how do YOU interpret the process of estimating a population parameter?
# - collect data to see if data match a pre-existing value (Frequentist)
# - collect data to collect information to estimate the value (Bayesian)

#' *samples*
# we cannot know information about an entire population over time
# but we can collect samples to have a partial picture of the population
samples <- tibble(sample_id = 1:12,
                  # generate a random sample of 11 masses for each id
                  sample_data = map(sample_id, function(.id) {
                    set.seed(100 + .id)
                    tibble(mass_kg = rnorm(11, mean = mu, sd = sigma))
                  })) %>%
  mutate(sample_id = paste('Sample', sample_id) %>%
           factor(., levels = unique(.))) %>% # keep sorted by number
  unnest(sample_data)
samples

p_samples <-
  ggplot(samples) +
  facet_wrap(~ sample_id) +
  geom_histogram(aes(mass_kg), binwidth = 2.5, color = 'black',
                 fill = 'grey') +
  labs(x = 'Mass (kg)', y = 'Count')
p_samples

#' *sample statistics*
# use the samples to calculate the sample mean and sample variance
sample_summaries <- samples %>%
  group_by(sample_id) %>%
  summarize(sample_mean = mean(mass_kg),
            sample_sd = sd(mass_kg),
            sample_var = sample_sd^2)

# sample means and SDs differ across samples
p_sds <-
  p_samples +
  geom_rect(aes(xmin = sample_mean - sample_sd, ymin = 0,
                xmax = sample_mean + sample_sd, ymax = Inf),
            sample_summaries, lty = 'dotted', fill = 'red', alpha = 0.2) +
  geom_vline(aes(xintercept = sample_mean), sample_summaries,
             color = 'red', lty = 'dashed') +
  ggtitle('Samples with sample mean \U00B1 1 SD')
p_sds

#' *estimates and sampling distributions*
# sample statistics are estimates of the population statistics:
#' - sample mean is an estimate of `mu`, called `x_bar` or `mu_hat`
#' - sample SD is an estimate of `sigma`, called `s` or `s_hat`
#' - sample var is an estimate of `mu`, called `s2` or `sigma2_hat`
# but there is randomness in the estimation process
# distribution of the 12 sample means
ggplot(sample_summaries) +
  geom_histogram(aes(sample_mean), binwidth = 0.75, color = 'black',
                 fill = 'grey') +
  labs(x = expression('Sample means,'~hat(mu)~(kg)),
       y = 'Count')

# distribution of the 12 sample SDs
ggplot(sample_summaries) +
  geom_histogram(aes(sample_sd), binwidth = 0.25, color = 'black',
                 fill = 'grey') +
  labs(x = expression('Sample standard deviations,'~hat(sigma)~(kg)),
       y = 'Count')

# distribution of the 12 sample variances
ggplot(sample_summaries) +
  geom_histogram(aes(sample_var), binwidth = 0.75, color = 'black',
                 fill = 'grey') +
  labs(x = expression('Sample variances,'~widehat(sigma^2)~(kg^2)),
       y = 'Count')

#' *uncertainty (standard deviation, variance, standard error)*
# we can use the variation within the sample to estimate the variance in
# the sample mean:
#' - sample SD: SD *of the data* in the sample; estimate of `sigma`
#' - sample var: variance of the data in the sample; estimate of `sigma^2`
#' - SE: SD *of the sample mean*; estimate of `sqrt(Var(mu_hat))`
all(sample_summaries$sample_sd^2 == sample_summaries$sample_var)

#' `SE = sqrt(Var(mu_hat)) = sqrt(sigma^2 / n) = sigma_hat / sqrt(n)`
sample_summaries <- sample_summaries %>%
  mutate(sample_se = sample_sd / sqrt(11)) # sample size is 11

sample_summaries # sample SEs are different across samples

# distribution of the 12 sample variances
ggplot(sample_summaries) +
  geom_histogram(aes(sample_se), binwidth = 0.075, color = 'black',
                 fill = 'grey') +
  labs(x = expression('Sample sandard errors in the mean,'
                      ~hat(sigma)[hat(mu)]~(kg)),
       y = 'Count')

#' *Frequentist hypothesis testing*
#' we can check if our data is consistent with a preexisting hypothesis
#' for example, animals from another population have a known mean of 12 kg.
#' does this population have a different mean?
#' if so, we would data to be consistent with this hypothesis
#' indicate the null hypothesis as H_0: `mu_0` = 12 kg
#' the alternative hypothesis could be H_a: `mu` != 12 kg
#' *Note:*
#' - we are not trying to estimate the parameters
#' - we are checking if the data are consistent with H_0

#' *confidence intervals*
#' - intervals with probability `(1 - alpha)` of containing true parameter
#' - 95% CI for `mu` have `P(L < mu < U) = 0.95`
#' - not a measure of uncertainty around `mu_hat`, especially with low n
#' - useful for hypothesis testing, but not particularly if n is large
# calculate two-sided 95% CIs
sample_summaries <-
  sample_summaries %>%
  mutate(lower_ci = sample_mean + qt(0.05 / 2, df = 11 - 1) * sample_se,
         upper_ci = sample_mean + qt(1 - 0.05 / 2, 11 - 1) * sample_se)

# histograms of the data with 95% CIs
p_cis <-
  p_samples +
  geom_rect(aes(xmin = lower_ci, ymin = 0,
                xmax = upper_ci, ymax = Inf),
            sample_summaries, lty = 'dotted', fill = 'red', alpha = 0.2) +
  geom_vline(aes(xintercept = sample_mean), sample_summaries,
             color = 'red', lty = 'dashed') +
  ggtitle('Samples with sample mean and 95% CI')
p_cis

#' Q: how does this figure differ from `p_sds`?
plot_grid(p_sds, p_cis, ncol = 1)

#' *p-values*
#' indicate the probability of obtaining this dataset or a more extreme one
#' under the null hypothesis
# one-sided p-values
densities_hyp_testing <-
  mutate(sample_summaries,
         dens_data = list(filter(densities, mass_kg >= 7.5, mass_kg <= 17.5))) %>%
  unnest(dens_data) %>%
  mutate(group = case_when(mass_kg < lower_ci - sample_mean + 12 ~ 'left',
                           mass_kg < upper_ci - sample_mean + 12 ~ 'center',
                           mass_kg >= upper_ci - sample_mean + 12 ~ 'right'),
         h_0_density = dt((mass_kg - 12) / (sigma / sqrt(11)), 11 - 1))

# two-sided p-values and CIs
ggplot() +
  facet_wrap(~ sample_id) +
  geom_area(aes(mass_kg, h_0_density),
            densities_hyp_testing, color = 'black', alpha = 0) +
  geom_area(aes(mass_kg, h_0_density, color = group, fill = group),
            densities_hyp_testing, alpha = 0.3) +
  geom_vline(aes(xintercept = 12), sample_summaries,
             color = 'black', lty = 'dashed') +
  geom_vline(aes(xintercept = sample_mean), sample_summaries,
             color = 'red', lty = 'dashed') +
  labs(x = 'Mass (kg)', y = 'Density') +
  scale_color_manual(values = c('black', 'red', 'red'),
                     aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

#' Q: how would the figure look if *H_a: mu > 12* or *H_a: mu < 12*?

#' *bias*
#' - the amount by which estimates are systematically off
#' `B(mu_hat) = E(mu_hat) - mu`
#' - e.g., small individuals evade capture => overestimation => bias > 0
#' - e.g., large individuals break traps => underestimation => bias < 0
#' ideally the bias is zero

#' *imprecision*
#' - spread of the estimates around the average estimate
#' - affected by variation and randomness in the data
#' - measured using SE if no bias is present

#' *inaccuracy*
#' - distance between the average estimate and the true parameter
#' - affected by sample bias
#' - will inflate the SE if nonzero


#' *NOTE:* sample SD, variance, and SE all include the sample bias

expand_grid(imprecision = c(0, 5),
            inaccuracy = c(0, 10)) %>%
  mutate(sample_data = map2(imprecision, inaccuracy, function(.impr, .inac) {
    tibble(x = rnorm(100, mean = mu + .inac, sd = sigma + .impr),
           y = rnorm(100, mean = mu + .inac, sd = sigma + .impr))
  })) %>%
  mutate(imprecision = if_else(imprecision == 0, 'Precise', 'Imprecise') %>%
           factor(., levels = unique(.)),
         inaccuracy = if_else(inaccuracy == 0, 'Accurate', 'Inaccurate') %>%
           factor(., levels = unique(.))) %>%
  unnest(sample_data) %>%
  ggplot() +
  facet_grid(inaccuracy ~ imprecision) +
  geom_point(aes(x, y), alpha = 0.2) +
  annotate(geom = 'point', x = mu, y = mu, color = 'red', size = 2.5)

#' *power*
#' probability of detecting a truly significant result
#' increases with effect size and sample size
#' not necessarily indicative of an important relationship or effect

# P(reject H_0 when H_0 is true) => false positive rate (type 1 error)
# P(fail to reject H_0 | H_0 is true) => true negative rate (true negative)
# P(reject H_0 when H_0 is false) => power (true positive)
# P(fail to reject H_0 | H_0 is false) => false negative rate (type 2 error)

#' *statistical significance vs biological relevance*
#' statistical significance increases with sample size
#' biological relevance is independent of sample size
#' biologically irrelevant effects can be statistically significant
#' e.g., as our sample size tends to infinity, the p-value will tend to 0

#' Q: what is `P(mu = mu_0)`?

#' Q: how does this affect your interpretation of hypothesis testing?

#' *dealing with missing data*
#' substitute missing data with averages:
#' - biases estimates towards the (random) sample average
#' - is not good practice

#' in a Bayesian framework, priors can be used to fill in missing data:
#' - added data have measures of uncertainty
#' - added data affect results proportionally to uncertainty
set.seed(95)
d_ts <- tibble(time = 1:100,
               y = rnorm(length(time))) %>%
  mutate(y = cumsum(y),
         y = y + min(y) + 10)

ggplot(d_ts) +
  geom_line(aes(time, y), alpha = 0.3) +
  geom_point(aes(time, y))

# Q: how would you perform data imputation on this time series?
d_ts %>%
  filter(time < 50 | time > 70) %>%
  ggplot() +
  geom_line(aes(time, y), alpha = 0.3) +
  geom_point(aes(time, y))

# data interpolation gets increasingly problematic as gaps get bigger
d_ts %>%
  filter(time < 40 | time > 80) %>%
  ggplot() +
  geom_line(aes(time, y), alpha = 0.3) +
  geom_point(aes(time, y))

# spoiler: GAMs smooth over gaps efficiently
d_ts %>%
  filter(time < 40 | time > 80) %>%
  ggplot(aes(time, y)) +
  geom_smooth(method = 'gam', color = 'darkorange', fill = 'darkorange',
              alpha = 0.3, lwd = 1, n = 1e3) +
  geom_point(aes(time, y))

#' *if review topics are still unclear, review them before next week*

#' ***break**

#' ========================================================================

# part 2: introduction to (linear) trend detection (ANOVA, ANCOVA) ----
d_1 <- gamSim(eg = 5, n = 100) %>% as_tibble()

#' *fitting linear models*
lm_1 <- gam(formula = y ~ x1, family = gaussian(), data = d_1)

# default plot
plot(lm_1) # fails because there are no smooth terms
plot(lm_1, all.terms = TRUE) # explicitly tell it to include linear terms

#' `ggplot2`-based plots with `gratia`
draw(lm_1) #' fails like `plot()`
draw(lm_1, parametric = TRUE)

#' *NOTE:* when `x1 = 0`, `y = 0`, and CIs are zero-width

# extract model coefficients
coef(lm_1)
#' `(Intercept)` is the *y intercept*: the value of `y` when `x1` is 0
#' `x1` is the *slope*: the change in `y` when `x1` increases by 1

#' Q: did `mu` (not `y`) change significantly?
#' check the significance table to see, given some alpha value
summary(lm_1)

#' `Family: gaussian`: `y` is assumed to be *conditionally* Gaussian
#' `Formula`: model formula
#' `Parametric coefficients`: table for intercept & slope coefs containing:
#'    - estimates
#'    - standard errors
#'    - t values (sometimes z values if variance = mean)
#'    - p values
#' `R-sq. (adj.)`: adjusted R^2
#' `Deviance explained`: percentage of deviance explained by the model
#' `GCV`: score from Generalized Cross Validation
#' `Scale est`: scale parameter (close to `var(resid(lm_1))`)
#' `n`: sample size 

#' *predicting with LMs*
head(predict(lm_1)) # returns predicted means
predict(lm_1, newdata = tibble(x1 = c(1, 3))) # can predict for other data
predict(lm_1, se.fit = TRUE, newdata = head(d_1)) # can include SE

#' `gratia::fitted_values()` returns predictions with SE and CIs
preds_1 <- tibble(x1 = seq(0, 1, length.out = 400)) %>%
  fitted_values(object = lm_1, # our model
                data = ., # the data from the line above
                ci_level = 0.95) # specify 95% CIs

p_preds_1 <-
  ggplot() +
  geom_point(aes(x1, y), d_1, alpha = 0.3) +
  geom_ribbon(aes(x1, ymin = .lower_ci, ymax = .upper_ci), preds_1,
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(x1, .fitted), preds_1, color = 'darkorange', linewidth = 1)
p_preds_1

# intercept
p_preds_1 +
  geom_point(aes(0, coef(lm_1)['(Intercept)']), color = 'dodgerblue3',
             size = 3) +
  geom_hline(yintercept = coef(lm_1)['(Intercept)'], color = 'dodgerblue3',
             lty = 'dashed')

# slope
p_preds_1 +
  geom_polygon(aes(x1, y),
               tibble(x1 = c(0, 1, 1),
                      y = c(0, coef(lm_1)[2], 0) + coef(lm_1)[1]),
               color = 'dodgerblue3', fill = 'dodgerblue3', alpha = 0.2,)

# check if change is significant: does the CI include the horizontal line?
p_preds_1 +
  geom_hline(yintercept = predict(lm_1, newdata = tibble(x1 = 0.5)),
             lty = 'dashed')

#' easier to see with `draw()` or `plot()`:
draw(lm_1, parametric = TRUE) &
  geom_hline(yintercept = 0, lty = 'dashed')

#' *signal, error in data, model error*
#' signal: trends we consider to be of interest
#' error in data: the noise in the data around the signal(s)
#' model error: the difference between the truth and the model
p_preds_1 +
  geom_smooth(aes(x1, y), d_1, alpha = 0.3)

# Q: how do you decide what counts as signal vs error?

# Q: how to distinguish randomness/errors in the data from in the model?

# Q: what kinds of trends can you detect with linear models?

#' `co2` (lowercase): a time series object, so convert it into a data frame
?co2
d_co2 <- tibble(dec_date = as.vector(time(co2)),
                year = floor(dec_date),
                season = dec_date - year,
                co2_ppm = as.vector(co2))
d_co2

lm_co2 <- gam(co2_ppm ~ dec_date, family = gaussian(), data = d_co2)

p_preds_co2 <-
  ggplot() +
  geom_point(aes(dec_date, co2_ppm), d_co2, alpha = 0.5) +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fitted_values(lm_co2), fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(dec_date, .fitted), fitted_values(lm_co2),
            color = 'darkorange', linewidth = 1)
p_preds_co2

# Q: would you answers to the three questions above change?

# Q: when would you say co2 increased significantly?

# Q: how would you detect a change in the slope?

#' *limitations of linear models*
appraise(lm_co2) # see nonlinear trends in residuals vs linear predictors

#' *polynomial models* can estimate some "nonlinear" trents
#' *NOTE:* still actually linear because the models are sums of products:
#'  `mu = b0 + b1 * x1 + b2 * (x1)^2 = b0 + b1 * x1 + b2 * x2`
lm_co2_poly <- gam(formula = co2_ppm ~ poly(dec_date, 2),
                   family = gaussian(), data = d_co2)

draw(lm_co2_poly, parametric = TRUE)

#' model now assumes a parabolic increase
#' still can't tell when change started: coefs apply for all values of `x1`
fitted_values(lm_co2_poly, data = d_co2) %>%
  ggplot() +
  geom_point(aes(dec_date, co2_ppm), d_co2, alpha = 0.5) +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(dec_date, .fitted), color = 'darkorange', linewidth = 1)

#' *interaction terms*
# polynomials can be interpreted as interaction terms:
#' - `(Intercept)`: `co2_ppm` when `dec_date` is zero
#' - `dec_date` coef: increase in `co2_ppm` in a year
#' - `dec_date^2` coef: change in the coef of `dec_date` with `dec_date`
#'                      `dec_date^2 = dec_date * dec_date`
#' 
#' the effect of `dec_date` depends on `dec_date`: `dec_date` has more of
#' an effect as `dec_date` increases

# we could also add a polynomial term of season
d_co2 %>%
  mutate(co2_ppm = co2_ppm - mean(co2_ppm), .by = 'year') %>%
  ggplot() +
  geom_line(aes(season, co2_ppm, group = year), alpha = 0.5)

# fit a model with season and interaction terms
lm_co2_int_1 <-
  gam(co2_ppm ~
        poly(year, 2) + # long-term trend
        poly(season, 4) + # seasonal trend
        poly(season, 3):poly(year, 2),# change in seasonal trend over years
      family = gaussian(), data = d_co2)

draw(lm_co2_int_1, parametric = TRUE) # plots with original data values
draw(lm_co2_int_1, parametric = TRUE, rug = FALSE,
     data = tibble(year = seq(1959, 1997, length.out = 400),
                   season = seq(0, 0.9166667, length.out = 400)))

p_int_1 <-
  fitted_values(object = lm_co2_int_1, data = d_co2) %>%
  ggplot() +
  geom_point(aes(dec_date, co2_ppm), d_co2, alpha = 0.5) +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(dec_date, .fitted), color = 'darkorange', linewidth = 1)
p_int_1

draw(lm_co2_int_1, parametric = TRUE,
     data = tibble(year = 1959:1997,
                   season = seq(0, 1, length.out = length(year))))

# 
lm_co2_int_2 <-
  gam(co2_ppm ~
        poly(year, 4) + # long-term trend
        poly(season, 4) + # seasonal trend
        poly(season, 4):poly(year, 4),# change in seasonal trend over years
      family = gaussian(), data = d_co2)

fitted_values(object = lm_co2_int_2, data = d_co2) %>%
  ggplot() +
  geom_point(aes(dec_date, co2_ppm), d_co2, alpha = 0.5) +
  geom_ribbon(aes(dec_date, ymin = .lower_ci, ymax = .upper_ci),
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(dec_date, .fitted), color = 'darkorange', linewidth = 1)

# many textbooks suggest performing model selection using p-values or AIC
AIC(lm_co2_poly,  # no season or interaction terms
    lm_co2_int_1, # with season and interaction terms
    lm_co2_int_2) # with more complex year and interaction terms

summary(lm_co2_int_2) # interaction may be unnecessary?

lm_co2_seas <- gam(co2_ppm ~ poly(year, 4) + poly(season, 4),
                   family = gaussian(), data = d_co2)

AIC(lm_co2_poly,  # no season or interaction terms
    lm_co2_int_1, # with season and interaction terms
    lm_co2_int_2, # with more complex year and interaction terms
    lm_co2_seas)  # with high flexibility but without interaction term

# how do we choose the complexity of the polynomial terms?
new_d_season <- tibble(season = seq(0, 1, length.out = 400))

plot_grid(draw(gam(co2_ppm ~ poly(season, 1), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 2), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 3), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 4), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 5), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 6), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 7), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 8), data = d_co2),
               parametric = TRUE, data = new_d_season),
          draw(gam(co2_ppm ~ poly(season, 9), data = d_co2),
               parametric = TRUE, data = new_d_season))

#' *extra questions*
#' Q: how do you relate the concept of a population with time series?
#' Q: how many samples do you need to estimate a trend?
#' Q: how many samples do you need to estimate a significant trend?

#' *extra work for those interested*
#' Q: how would you model the three datasets below?
#' Q: how does using linear models restrict you?
#' Q: what would you do to overcome the models' limitations?

#' `ChickWeight`: weight of chicks over time (0-21) eating 4 diets
ChickWeight <- janitor::clean_names(ChickWeight) %>% as_tibble()
ChickWeight

ggplot(ChickWeight, aes(time, weight, group = chick)) +
  facet_wrap(~ diet) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.3)

#' `CO2` (all uppercase; see `?CO2` for more info)
#' dataset on CO2 uptake by plants (Cockspur grass; Echinochloa crus-galli)
?CO2

CO2 <- janitor::clean_names(CO2) # use tidyverse syntax for names

ggplot(CO2, aes(conc, uptake, group = plant)) +
  facet_grid(treatment ~ type) +
  geom_line() +
  geom_point(alpha = 0.3)

#' `ouf_sim`: sample date along with concentrations of two compounds
ouf_sim <- readr::read_csv('data/ouf-sim.csv', col_types = 'Ddd')

ouf_sim %>%
  pivot_longer(! date) %>%
  ggplot(aes(date, value)) +
  facet_wrap(~ name, scale = 'free_y') +
  geom_line() +
  geom_point(alpha = 0.3) +
  geom_smooth(method ='lm', color = 'darkorange', fill = 'darkorange')
