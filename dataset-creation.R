library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ctmm')    # for continuous-time stochastic processes
library('ggplot2') # for fancy plots

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

readr::write_csv(ouf_sim, file = 'data/ouf-sim.csv')
readr::read_csv('data/ouf-sim.csv', col_types = 'Ddd')
