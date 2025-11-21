library('dplyr')   #' for data wrangling
library('tidyr')   #' for data wrangling
library('ctmm')    #' for continuous-time stochastic processes
library('ggplot2') #' for fancy plots
library('mgcv')    #' for `gamSim()`
library('readr')   #' for saving csv files
library('gratia')  #' for plotting GAMs and their diagnostics
theme_set(theme_bw())

# OUF simulation ----
mm <- ctmm(tau = c(100, 3), isotropic = FALSE, mu = c(0, 0), sigma = 1)

ouf_sim <-
  simulate(mm, t = 1:1e3) %>%
  data.frame() %>%
  as_tibble() %>%
  transmute(date = as.Date('2023-05-07') + t,
            compound_1 = (x - min(x) + 1) * 12,
            compound_2 = (y - min(y) + 0.25) * 167)

ggplot(ouf_sim, aes(compound_1, compound_2)) + geom_point()

ouf_sim %>%
  pivot_longer(! date) %>%
  ggplot(aes(date, value)) +
  facet_wrap(~ name, scale = 'free_y') +
  geom_line() +
  geom_point(alpha = 0.3)

write_csv(ouf_sim, file = 'data/ouf-sim.csv')
read_csv('data/ouf-sim.csv', col_types = 'Ddd')

# simulated data for yers since disturbance ----
set.seed(2)
d_disturbance <- gamSim(eg = 4, n = 1000) %>%
  transmute(perc_forest = x0 * 100,
            elevation_m = x1 * 200 + 750,
            years = x2 * 25,
            site = factor(paste('Site', fac)),
            animals_per_km2 = (rpois(n(), lambda = y - min(y) + 1) + 1) / 2) %>%
  as_tibble()
d_disturbance

m <- gam(animals_per_km2 ~
           s(perc_forest, by = site) +
           s(elevation_m, by = site) +
           s(years, by = site),
         family = tw(link = 'log'), data = d_disturbance, method = 'REML')
draw(m)
appraise(m, point_alpha = 0.3)

write_csv(d_disturbance, 'data/disturbance-data.csv')
read_csv('data/disturbance-data.csv', col_types = 'dddfd')
