library('ggplot2')

plot_acf <- function(.model, max_lag = 100) {
  acf(resid(.model), plot = FALSE, lag.max = max_lag) %>%
    with(data.frame(lag = lag, acf = acf)) %>%
    ggplot() +
    annotate('rect', color = 4, fill = 4, alpha = 0.3,
             xmin = -Inf, ymin = qnorm(0.025)/sqrt(nrow(d_ouf)),
             xmax = Inf, ymax = qnorm(0.975)/sqrt(nrow(d_ouf))) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x = lag, y = 0, xend = lag, yend = acf), lwd = 1)
}

