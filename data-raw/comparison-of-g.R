# caclulations

mu <- 3
sigma <- 0.2

# calculate the log-normal

1 / (2 * dlnorm(qlnorm(0.5, mu, sigma),
                mu,
                sigma))

# use g

library(varameta)

g_lnorm(1,
        qlnorm(0.5, mu, sigma),
        diff(qlnorm(c(0.25, 0.75), mu, sigma)),
        spread_type = 'iqr')


# instantiate dataset

library(simeta)
library(tidyverse)
library(latex2exp)


plotdat <-
  default_parameters %>%
  mutate(
    median =
      map2_dbl(dist, par, density_fn, x = 0.5, type = "q"),
    iqr = map2_dbl(dist, par, density_fn, x = 0.75, type = "q") -
      map2_dbl(dist, par, density_fn, x = 0.25, type = "q"),
    var = pmap_dbl(
      list(dist, par, median),
      .f = function(dist, par, nu) {
        1 / (2 *
               density_fn(
                 x = nu,
                 distribution = dist,
                 parameters = par,
                 type = "d"
               ))
      }
    ),
    exp = map_dbl(median, .f = g_exp, n = 1) / var,
    norm = map2_dbl(median, iqr, .f = g_norm, n = 1) / var,
    lnorm = map2_dbl(median, iqr, .f = g_lnorm, n = 1) / var,
    cauchy = map2_dbl(median, iqr, .f = g_cauchy, n = 1) / var
  ) %>%
  select(-median,-iqr,-var) %>%
  # ugh ugh why is this not fucking working?
  pivot_longer(c(exp, norm, lnorm, cauchy),
               names_to = "g",
               values_to = "ratio")


g_plot <- function (plotdat) {
  plotdat %>%
    ggplot(aes(x = g, y = ratio, colour = dist)) +
    geom_jitter(alpha = 0.6, size = 4) +
    hrbrthemes::scale_color_ipsum("True density") +
    labs(title = "Precision of different subsitutions of density",
         x = TeX("$g$"),
         y = TeX("$\\rho$"))
}

plotdat %>%
  g_plot()


plotdat %>%
  dplyr::filter(g != "exp") %>%
  g_plot()
