#' day 1: 2026-02-03
# attach necessary packages
library('dplyr')     # for data wrangling
# library('tidyr')     # for data wrangling
# library('purrr')     # for functional programming
# library('lubridate') # for working with dates
library('ggplot2')   # for fancy plot
library('ggExtra')   # for marginal histograms
library('mgcv')      # for modeling
library('gratia')    # for plotting models
# library('cowplot')   # for multi-panel plots
theme_set(theme_bw())

chick_weight <- janitor::clean_names(ChickWeight) %>% as_tibble()
chick_weight

ggplot(chick_weight, aes(time, weight, group = chick)) +
  facet_wrap(~ diet) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.3)

# part 1:  ----
# linear models fit poorly to exponential growth data
ggplot(chick_weight, aes(time, weight)) +
  facet_wrap(~ diet) +
  geom_line(aes(group = chick), alpha = 0.5) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', formula = y ~ x,
              color = 'darkorange', fill = 'darkorange', alpha = 0.3)

#' many would log-transform the data, but this doesn't fix the issues... 
ggplot(chick_weight, aes(time, log(weight))) +
  facet_wrap(~ diet) +
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

#' moving beyond Gaussian models: *how to choose a family of distributions*
#' **fit the model to the data, not the data to the model!**
?stats::family

#' `binomial(link = "logit")`: binary (0/1 data; mean is `p = P(success)`)
#' `gaussian(link = "identity")`: all real numbers (-Inf, Inf)
#' `Gamma(link = "log")`: strictly positive; no zeros (0, Inf)
#' `poisson(link = "log")`: count data (positive integers including zero)

?mgcv::family.mgcv


# other families
#' `inverse.gaussian(link = "log")`: strictly positive; uncommon
#' `quasibinomial(link = "logit")`: 
#' `quasipoisson(link = "log")`: 

#' *link functions*


#' *fitting Generalized Linear Models*


#' *model diagnostics*


#' *limitations of GLMs*


#' ========================================================================

# Part 2 ----
#' *fitting Generalized Additive Models*


#' *interpreting nonlinear terms*


#' *selecting model complexity*


#' *modeling seasonal and daily trends*


#' *model diagnostics*

