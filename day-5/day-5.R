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
  mutate(chick = factor(chick), # remove ordering in the factors
         # add percent protein for each diet
         protein_percent = case_when(diet == 1 ~ 0,
                                     diet == 2 ~ 10,
                                     diet == 3 ~ 20,
                                     diet == 4 ~ 40))

#' modeling data from multiple sites/individuals
ggplot(chick_weight, aes(time, weight, group = chick)) +
  facet_wrap(~ diet) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.3)

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

#' best model from day 2: only for diet 1; does not account for individuals 
m_cw_0 <- gam(formula = weight ~ s(time), # weight varies smoothly w time
              family = Gamma(link = 'log'), # weight > 0
              data = chick_weight,
              subset = diet == 1, # subset to only diet 1
              method = 'REML') # method for smoothness selection
draw(m_cw_0, residuals = TRUE) # clearly does not account for individuals
appraise(m_cw_0, point_alpha = 0.3) # diagnostics are not great

#' fit a model with data for all 4 diets
m_cw_1 <- gam(formula = weight ~ s(time),
              family = Gamma(link = 'log'),
              data = chick_weight, #' dropped `subset = diet == 1` 
              method = 'REML')
draw(m_cw_1, residuals = TRUE) # clearly does not account for individuals
appraise(m_cw_1, point_alpha = 0.3) # diagnostics are even worse

#' use a factor smooth for each chick
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
  geom_point(aes(fitted(m_cw_2), fitted(m_cw_3)), alpha = 0.3)

draw(m_cw_3)

#' Q: how would you assess common trends for a specific diet?
#' **HERE**
m_cw_4 <- bam(weight ~
                s(time) + # common smooth effect of time
                s(time, diet, bs = 'fs') + #'deviations from common `s()`
                s(time, chick, bs = 'fs') + #' chick-level deviations
                diet, 
              family = Gamma(link = 'log'), # weight is > 0
              data = chick_weight,
              method = 'REML', # fast REML
              discrete = TRUE) # requires fREML


m_cw_x <- bam(weight ~
                s(time) + # common smooth effect of time
                s(time, diet, bs = 'fs') + #'deviations from common `s()`
                s(time, chick, bs = 'fs') + #' chick-level deviations
                diet, 
              family = Gamma(link = 'log'), # weight is > 0
              data = chick_weight,
              method = 'REML', # fast REML
              discrete = TRUE) # requires fREML

#'
#' **break** --------------------------------------------------------------
#' 
#' experimental design and inference
#' BACI design
#' control and treatment sites
#' `by` binary smooths
#' assuming site 1
#' is a control site, and sites 2 and 3 use different recovery procedures
#' after disturbance. 
d_dist <- read_csv('data/disturbance-data.csv', col_types = 'dddcd') %>%
  mutate(site = factor(site), #' sites are out of order if `col_type = 'f'`
         disturbed = if_else())

density_lab <- expression(Population~density~(km^'-2'))

# exploratory plot
d_dist %>%
  pivot_longer(c(forest_perc, elevation_m, years)) %>%
  mutate(name = case_when(name == 'forest_perc' ~ 'Forest cover (%)',
                          name == 'elevation_m' ~ 'Elevation (m a.s.l.)',
                          name == 'years' ~ 'Years since disturbance')) %>%
  ggplot(aes(value, animals_per_km2)) +
  facet_grid(site ~ name, scales = 'free_x', switch = 'x') +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x), color = 'darkorange',
              fill = 'darkorange') +
  labs(x = NULL, y = density_lab) +
  theme(strip.placement.x = 'outside', strip.background.x = element_blank(),
        strip.text.x = element_text(size = 11))

m <- gam(animals_per_km2 ~ s(years, by = ))

#' predicting out of sample (random effects)
#' forecasting (leveraging hierarchical structure)


#' *extra work for those interested*
#' repeat the analyses on significant rates of change from day 4 using the
#' models and data for all three sites. Think carefully about what plots
#' best convey the results and what terms you should predict for when
#' creating the plots.

#' *extra materials for those interested*
#' - `https://peerj.com/articles/6876/`
#' - `https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus`
#' - `https://xcelab.net/rm/`
#' 
