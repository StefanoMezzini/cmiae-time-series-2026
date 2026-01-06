library('dplyr')   #' for data wrangling
library('tidyr')   #' for data wrangling
library('ctmm')    #' for continuous-time stochastic processes
library('ggplot2') #' for fancy plots
library('mgcv')    #' for `gamSim()`
library('readr')   #' for saving csv files
library('gratia')  #' for plotting GAMs and their diagnostics
theme_set(theme_bw())

# OUF simulation ----
set.seed(50)
d_sim <- tibble(date = as.Date('2023-05-07') + 1:1e3,
                x = lubridate::decimal_date(date) - 2024) %>%
  mutate(mu = (x * (x+1)^2 * (x-2)^3) / 3 + 3.5,
         e = rnorm(length(date), sd = 0.1))

# add correlated trends
for(i in 1:nrow(d_sim)) {
  if(i > 1) {
    d_sim$e[i] <- d_sim$e[i] + 0.5 * d_sim$e[i - 1]
  }
  if(i > 2) {
    d_sim$e[i] <- d_sim$e[i] + 0.4 * d_sim$e[i - 2]
  }
}

d_sim <- d_sim %>%
  mutate(conc = exp(mu + e)) %>%
  select(! c(x, e, mu))

ggplot(d_sim, aes(date, conc)) + geom_point()

plot(acf(d_sim$conc))
plot(pacf(d_sim$conc))

write_csv(d_sim, file = 'data/conc-sim.csv')
read_csv('data/conc-sim.csv', col_types = 'Ddd')

# simulated data for yers since disturbance ----
set.seed(2)
d_disturbance <- gamSim(eg = 4, n = 1000) %>%
  transmute(forest_perc = x0 * 100,
            elevation_m = x1 * 200 + 750,
            years = x2 * 25,
            site = factor(paste('Site', fac)),
            animals_per_km2 = (rpois(n(), lambda = y - min(y) + 1) + 1) / 2) %>%
  as_tibble()
d_disturbance

m <- gam(animals_per_km2 ~
           s(forest_perc, by = site) +
           s(elevation_m, by = site) +
           s(years, by = site),
         family = tw(link = 'log'), data = d_disturbance, method = 'REML')
draw(m)
appraise(m, point_alpha = 0.3)

write_csv(d_disturbance, 'data/disturbance-data.csv')
read_csv('data/disturbance-data.csv', col_types = 'dddfd')

# add control years before the disturbance to the data ----
d_disturbance_previous <-
  read_csv('data/disturbance-data.csv', col_types = 'dddcd') %>%
  mutate(site = factor(site)) %>%
  filter(years <= 15) %>%
  group_by(site) %>%
  mutate(
    lambda_0 = mean(animals_per_km2[years < 3]),
    animals_per_km2 = rpois(n(), lambda_0),
    years = - seq(0, max(years), length.out = n())) %>%
  select(-lambda_0)

d_disturbance_2 <- bind_rows(d_disturbance_previous, d_disturbance) %>%
  mutate(disturbed = if_else(years < 0 |site == 'Site 1', 0, 1), # has disturbance
         treated = if_else(site == 'Site 3', 1, 0)) # faster recovery

ggplot(d_disturbance_2, aes(years, animals_per_km2)) +
  facet_grid(site ~ .) +
  geom_line(aes(color = years > 0, group = 1)) +
  scale_color_brewer(type = 'qual', palette = 6) +
  theme(legend.position = 'top')

m <- bam(animals_per_km2 ~
           s(forest_perc, by = site) +
           s(elevation_m, by = site) +
           s(years, by = site),
         family = tw(link = 'log'), data = d_disturbance_2,
         method = 'fREML', discrete = TRUE)
draw(m)
appraise(m, point_alpha = 0.1)

write_csv(d_disturbance_2, 'data/disturbance-data-full.csv')
read_csv('data/disturbance-data-full.csv', col_types = 'dddfd')
