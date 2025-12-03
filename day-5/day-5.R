#' Day 5: *Hierarchical Generalized Additive Models*
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

# chick weight data used on days 1 and 2, but now with all 4 diets
chick_weight <- janitor::clean_names(ChickWeight) %>%
  as_tibble() %>%
  mutate(chick = factor(chick, ordered = FALSE), # remove ordering
         # add percent protein for each diet
         protein_percent = case_when(diet == 1 ~ 0,
                                     diet == 2 ~ 10,
                                     diet == 3 ~ 20,
                                     diet == 4 ~ 40))

#' modeling data from multiple sites/individuals
ggplot(chick_weight, aes(time, weight, group = chick)) +
  facet_wrap(~ diet) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.3) +
  labs(x = 'Days since hatching', y = 'Mass (g)')

#' Q: what are some advantages of having a longitudinal dataset with
#'    multiple individuals?
#' Q: some individuals in diets 1 and 4 don't have data until day 21. what
#'    can we leverage to predict the missing data?

#' *fixed vs random effects*
#' the distinction between the two is often explained poorly. the best way
#' to distinguish between them is to think of data in terms of independence
#' *fixed effects*:
#' - estimate coefficients
#' - assume independence across coefficients
#' - do not allow prediction of individuals that were not in the data
#' *ramdom effects*:
#' - estimate distributions (generally Gaussian) with a mean and a variance
#' - assume individuals are sampled from a distribution
#' - allow for predictions of individuals that were not in the data, with
#'   appropriate measures of uncertainty
#' *note:* there is no minimum number of individuals you need to fit REs.
#'         some sources will say >= 5 individuals, but you can fit REs
#'         with as few as two individuals. see REs as a method for model
#'         shrinkage and assuming common effects rather than a large
#'         distribution.

# best model from day 2: only for diet 1; does not account for individuals 
m_cw_0 <- gam(formula = weight ~ s(time), # weight varies smoothly w time
              family = Gamma(link = 'log'), # weight > 0
              data = chick_weight,
              subset = diet == 1, # subset to only diet 1
              method = 'REML') # method for smoothness selection
draw(m_cw_0, residuals = TRUE) # clearly does not account for individuals
appraise(m_cw_0, point_alpha = 0.3) # diagnostics are not great

# fit a model with data for all 4 diets (not accounting for individuals)
m_cw_1 <- gam(formula = weight ~ s(time),
              family = Gamma(link = 'log'),
              data = chick_weight, #' dropped `subset = diet == 1` 
              method = 'REML')
draw(m_cw_1, residuals = TRUE) # clearly does not account for individuals
appraise(m_cw_1, point_alpha = 0.3) # diagnostics are even worse

# use a factor smooth for each chick
?mgcv::smooth.construct.fs.smooth.spec
m_cw_2 <- gam(weight ~ s(time, chick, bs = 'fs'),
              family = Gamma(link = 'log'),
              data = chick_weight,
              method = 'REML')
draw(m_cw_2) # account for individuals
appraise(m_cw_2, point_alpha = 0.3) # much better but underdispersed

#' this last model was much slower to fit. to speed things up without
#' losing model performance, we can do three things:
#' 1. use `bam()`: like `gam()` but for big datasets
#' 2. use `method = 'fREML'`: like REML, but faster
#' 3. use `discrete = TRUE`: discretizes covariates for faster prediction
#' note: `discrete = TRUE` requires `method = 'fREML'`
m_cw_3 <- bam(weight ~ s(time, chick, bs = 'fs'),
              family = Gamma(link = 'log'),
              data = chick_weight,
              method = 'fREML',
              discrete = TRUE)

# the predictions from the two models have a 1:1 relationship
ggplot() +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  geom_point(aes(fitted(m_cw_2), fitted(m_cw_3)), alpha = 0.3) +
  labs(x = 'Fitted values for gam() model',
       y = 'Fitted values for bam() model')

draw(m_cw_3)

#' Q: how would you assess common trends for a specific diet?

#' add a global smooth term (warning is ok)
m_cw_4 <- bam(weight ~
                s(time) + # global smooth effect of time
                s(time, chick, bs = 'fs'), # chick-level deviations
              family = Gamma(link = 'log'),
              data = chick_weight,
              method = 'fREML',
              discrete = TRUE)
draw(m_cw_4)
appraise(m_cw_4, point_alpha = 0.3) # residuals are still underdispersed

#' account for similarities and differences across diets
m_cw_5 <- bam(weight ~
                s(time) +
                s(time, diet, bs = 'fs') + # diet-level smooth 
                s(time, chick, bs = 'fs'), 
              family = Gamma(link = 'log'),
              data = chick_weight,
              method = 'fREML',
              discrete = TRUE)
draw(m_cw_5) # diet smooths were shrunken to linear terms
appraise(m_cw_5, point_alpha = 0.3) # residuals are still underdispersed

#' allow each diet to have a separate smoothness parameter
#' diets are allowed to have different levels of wiggliness
m_cw_5 <- bam(weight ~
                diet + #' intercepts are not included in `by` smooths
                s(time, by = diet) + # diet-level smooth 
                s(time, chick, bs = 'fs'), 
              family = Gamma(link = 'log'),
              data = chick_weight,
              method = 'fREML',
              discrete = TRUE)
draw(m_cw_5) # diet smooths were shrunken to linear terms
appraise(m_cw_5, point_alpha = 0.3) # residuals are still underdispersed

#' there are many different types of models you can set up:
#' see figures 2 and 4 of `https://peerj.com/articles/6876/`
#' the `diet` intercept could even be a RE, even if the smooth is `by` diet

# add a global smooth
m_cw_tw <-
  bam(weight ~ diet + s(time, by = diet) + s(time, chick, bs = 'fs'), 
      family = tw(link = 'log'), # to account for underdispersion
      data = chick_weight,
      method = 'fREML',
      discrete = TRUE)
draw(m_cw_tw)
appraise(m_cw_tw, point_alpha = 0.3) # QQ plot is not perfect, but it's ok

# we can also use a continuous variable to account for differences in diet
m_cw_tw_2 <-
  bam(weight ~
        s(time) +
        s(time, chick, bs = 'fs') +
        s(protein_percent, k = 3) +
        ti(time, protein_percent, k = c(10, 3)), 
      family = tw(link = 'log'), # to account for underdispersion
      data = chick_weight,
      method = 'fREML',
      discrete = TRUE)

#' the terms are arguably more interpretable than with diets as factors,
#' as this model directly estimates the effect of protein.
draw(m_cw_tw_2, dist = 1)
#' `s(protein_percent)`: protein-rich food increases the chicks' masses;
#' this is like an improved version of `s(diet, bs = 're')`. 
#' `ti(time, protein_percent)` chicks with less protein in their diet have
#' a flatter growth curve than the marginal smooth of `s(time)`, while
#' chicks with more protein have a steeper growth curve.

#' **break** --------------------------------------------------------------

#' experimental design and inference
summary(m_cw_tw_2)
#' `(Intercept)`: mean across the effects of `time` and `protein_percent`
#' `s()` terms are the marginal effects, i.e., effects for the variable
#'   after setting all other variables to the mean effect across the smooth
#' `ti()` is the change the marginal effect of `time` with `protein_percent`
#' and vice-versa

summary(m_cw_tw)
#' `(Intercept)` is the mean weight across time for the control group
#' `diet2` is the difference between the second diet and `(Intercept)`
#' `diet3` is the difference between the third diet and `(Intercept)`
#' `diet4` is the difference between the fourth diet and `(Intercept)`
#' each `s(time):dietX` is an independent smooth of time with a separate
#'   smoothness parameter for each `diet`; significance indicates
#'   significant
#' `s(time,chick)` is the group of `chick`-level random smooths of time;
#'   significance indicates significant variation across groups. may also
#'   indicate significant change if a group-level smooth is not included.
#' *note:* significance is approximate, and p-values are likely too small
#' because they neglect smoothing parameter uncertainty.

#' *predicting out of sample (random effects)*
# predict for a chick not in the sample for each diet
# predictions follow the data closely
chick_weight %>%
  filter(chick == first(chick), .by = 'diet') %>%
  fitted_values(object = m_cw_tw, data = ., scale = 'response') %>%
  ggplot(aes(time, .fitted, group = chick)) +
  facet_wrap(~ diet) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = 'darkorange',
              alpha = 0.3) +
  geom_line(color = 'darkorange', lwd = 1) +
  geom_point(aes(y = weight), chick_weight, alpha = 0.1) +
  geom_point(aes(y = weight), alpha = 0.5) +
  labs(x = 'Days since hatching', y = 'Mass (g)')

# predicting for only a chick from diet 1:
# - uncertainty is greater for the diets the chick didn't eat
# - estimated mean is somewhat above the group trend because the chick was
#   above the group trend in diet 1
data_slice(m_cw_tw,
           diet = unique(diet),
           time = evenly(time, 400),
           chick = filter(chick_weight, diet == 1)$chick[1]) %>%
  fitted_values(object = m_cw_tw, data = ., scale = 'response') %>%
  ggplot(aes(time, .fitted, group = chick)) +
  facet_wrap(~ diet) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = 'darkorange',
              alpha = 0.3) +
  geom_line(color = 'darkorange', lwd = 1) +
  geom_point(aes(y = weight), chick_weight, alpha = 0.3) +
  labs(x = 'Days since hatching', y = 'Mass (g)')

# predict for a new chick not in any of the diets
# - uncertainty is greater for diets with greater variation
# - estimated means follow the group-level trends
data_slice(m_cw_tw,
           diet = unique(diet),
           time = evenly(time, 400),
           chick = 'new chick') %>%
  fitted_values(object = m_cw_tw, data = ., scale = 'response') %>%
  ggplot(aes(time, .fitted, group = chick)) +
  facet_wrap(~ diet) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = 'darkorange',
              alpha = 0.3) +
  geom_line(color = 'darkorange', lwd = 1) +
  geom_point(aes(y = weight), chick_weight, alpha = 0.3) +
  labs(x = 'Days since hatching', y = 'Mass (g)')

#' *predicting beyond the range of the data*
# find chicks that do not have data up to day 21
chick_weight_short <- chick_weight %>%
  mutate(max_t = max(time), .by = chick) %>%
  filter(max_t < 21)

unique(chick_weight_short$chick)

#' predicting based on the data from one chick alone gives bad predictions
#' the GAM may not even be able to fit if data are too few
ggplot(chick_weight_short, aes(time, weight)) +
  facet_wrap(~ diet + chick) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 5), fullrange = TRUE,
              color = 'darkorange', fill = 'darkorange') +
  geom_point(alpha = 0.5) +
  labs(x = 'Days since hatching', y = 'Mass (g)')

# leverage the hierarchical structure to inform predictions
chick_weight_short %>%
  slice(1, .by = chick) %>%
  mutate(time = list(tibble(time = seq(0, 21, length.out = 400)))) %>%
  unnest(time) %>%
  fitted_values(object = m_cw_tw, data = ., scale = 'response') %>%
  ggplot(aes(x = time)) +
  facet_wrap(~ paste0('Diet ', diet, '; chick ', chick)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), fill = 'darkorange',
              alpha = 0.3) +
  geom_line(aes(y = .fitted), color = 'darkorange', lwd = 1) +
  geom_point(aes(time, weight), chick_weight_short, alpha = 0.5) +
  labs(x = 'Days since hatching', y = 'Mass (g)')
#' *careful!* the predictions the model produces will often tend towards
#' the group trends, which may not be appropriate: chicks in top row are
#' expected to start growing faster despite having being removed from the
#' experiment (likely due to poor health)

#' **break** --------------------------------------------------------------

#' *BACI design*:
#' - Before: start sampling before the treatment for a baseline
#' - After: continue sampling after the treatment to estimate change
#' - Control: keep a group undisturbed to account for changes in conditions
#' - Impact: assess the effect of the treatment relative to the control

#' example dataset:
#' - Site 1: control site
#' - Site 2: disturbed site
#' - Site 3: disturbed site with a treatment for faster recovery
#' the response is the number of animals / km^2 of an invasive species
d_dist <-
  read_csv('data/disturbance-data-full.csv', col_types = 'dddcd') %>%
  mutate(site = factor(site, paste('Site', 1:3)), # to have correct order
         disturbed_fac = factor(disturbed),
         treated_fac = factor(treated))

density_lab <- expression(Population~density~(km^'-2')) # label for plots

# exploratory plot
d_dist %>%
  pivot_longer(c(forest_perc, elevation_m, years)) %>%
  mutate(name = case_when(name == 'forest_perc' ~ 'Forest cover (%)',
                          name == 'elevation_m' ~ 'Elevation (m a.s.l.)',
                          name == 'years' ~ 'Years since disturbance')) %>%
  ggplot(aes(value, animals_per_km2)) +
  facet_grid(site ~ name, scales = 'free_x', switch = 'x') +
  geom_vline(aes(xintercept = x), lty = 'dashed',
             data = tibble(x = 0, name = 'Years since disturbance')) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x), color = 'darkorange',
              fill = 'darkorange') +
  labs(x = NULL, y = density_lab) +
  theme(strip.placement.x = 'outside', strip.background.x = element_blank(),
        strip.text.x = element_text(size = 11))

#' model each site separately using `by` factor smooths
m_dist_fe_1 <- bam(animals_per_km2 ~
                     site + # fixed-effect intercept
                     s(forest_perc, by = site) + # fixed-effect smooths
                     s(elevation_m, by = site) +
                     s(years, by = site),
                   family = tw(link = 'log'),
                   data = d_dist,
                   method = 'fREML',
                   discrete = TRUE)
draw(m_dist_fe_1, scales = 'fixed')
summary(m_dist_fe_1)
# each site is modeled independently, so it's hard to say what the effects
# of the disturbance and treatment are. significance indicates significant
# effect (no comparisons across sites)

# use random effects for each site
m_dist_re_1 <- bam(animals_per_km2 ~
                     s(site, bs = 're') + # random intercept
                     s(forest_perc, site, bs = 'fs') + # random smooths
                     s(elevation_m, site, bs = 'fs') +
                     s(years, site, bs = 'fs'),
                   family = tw(link = 'log'),
                   data = d_dist,
                   method = 'fREML',
                   discrete = TRUE)
draw(m_dist_re_1, scales = 'fixed')
summary(m_dist_re_1)
# sites are modeled together, but we can't separate the disturbance effect
# from that of the treatment. significance indicates significant difference
# across sites (at least 2 are different)

# use random effects for undisturbed/disturbed and untreated/treated
m_dist_re_2 <- bam(animals_per_km2 ~
                     # random intercepts
                     s(site, bs = 're') +
                     s(disturbed_fac, bs = 're') +
                     s(treated_fac, bs = 're') +
                     # random smooths
                     s(forest_perc, by = site) +
                     s(elevation_m, by = site) +
                     s(years, disturbed_fac, bs = 'fs') +
                     s(years, treated_fac, bs = 'fs'),
                   family = tw(link = 'log'),
                   data = d_dist,
                   method = 'fREML',
                   discrete = TRUE)
draw(m_dist_re_2, scales = 'fixed')
summary(m_dist_re_2)

#' use `by` binary variable for explicit contrasts
z <- d_dist %>%
  mutate(site = factor(paste(site, disturbed, treated)))
unique(z$site)

m_dist_fe_2 <- bam(animals_per_km2 ~
                     # random smooths of confounding variables
                     s(forest_perc, site, bs = 'fs') +
                     s(elevation_m, site, bs = 'fs') +
                     # year-specific smooths
                     s(years) + # reference smooth (control)
                     s(years, site, bs = 'fs') +
                     s(years, by = disturbed) + # effect of disturbance
                     s(years, by = treated), # effect of treatment
                   family = tw(link = 'log'),
                   data = z,
                   # data = d_dist,
                   method = 'fREML',
                   discrete = TRUE)
draw(m_dist_fe_2)
draw(m_dist_fe_2, select = 3:5) & geom_vline(xintercept = 0, lty='dashed')
summary(m_dist_fe_2)
#' `(Intercept)` is the mean response for the control, averaged across
#'   the smooth effects of `forest_perc`, `elevation_m`, and `years`
#' *note:* not across the average `forest_perc`, `elevation_m`, and `years
#' `siteSite 2` is the difference in the intercept and site 2's mean
#' `siteSite 3` is the difference in the intercept and site 3's mean
#' `s(forest_perc,site)` is the set of random smooths for `forest_perc`.
#'   all three smooths have the same smoothness parameter.
#' `s(elevation_m,site)` is the set of random smooths for `elevation_m`.
#'   all three smooths have the same smoothness parameter.
#' `s(years)` is the estimated effect of 
#'  
#' *note:* significance is approximate, and p-values are likely too small
#' because they neglect smoothing parameter uncertainty.

#' *extra work for those interested*
#' repeat the analyses on significant rates of change from day 4 using the
#' models and data for all three sites. Think carefully about what plots
#' best convey the results and what terms you should predict for when
#' creating the plots.
#' 
#' try applying the techniques I shared with you today to your data, even
#' an analysis you ran in the past.

#' *extra materials for those interested*
#' - `https://peerj.com/articles/6876/`
#' - `https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus`
#' - `https://xcelab.net/rm/`
#' - `https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop`
