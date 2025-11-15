# Day 2. Feb 3
# 
# Introduction to nonlinear trend detection: Part I: moving beyond Gaussian (Normal) data, link functions, fitting Generalized Linear Models, model diagnostics, limitations of GLMs.
# 
# Part II: fitting Generalized Additive Models, interpreting nonlinear terms, selecting model complexity, modeling seasonal and daily trends, model diagnostics.

?CO2

CO2 <- janitor::clean_names(CO2) # use  tidyverse syntax for names

#' modeling the `CO2` dataset...
#' ... with linear models
ggplot(CO2, aes(log(conc), uptake, group = plant)) +
  facet_grid(treatment ~ type) +
  geom_line() +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkorange')

#' with linear models and log-transforming the response
ggplot(CO2, aes(log(conc), uptake, group = plant)) +
  facet_grid(treatment ~ type) +
  geom_line() +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkorange')



# part 1:  ----
#' *do not transform response data nonlinearly*: Jensen's inequality

#' moving beyond Gaussian models: *how to choose a family of distributions*

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

