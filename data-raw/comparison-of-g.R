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
               values_to = "ratio") %>%
  mutate(distribution = pmap_chr(
    list(dist, par),
    .f = dist_label,
  )) %>%
  # for shape by Distribution (given a Family)
  group_by(dist, g) %>%
  mutate(distcat = 1:n())


distribution_levels <-
  plotdat %>%
  ungroup %>%
  select(distribution, distcat) %>%
  distinct() %>%
  group_split(distcat) %>%
  map(1) %>%
  map(paste, collapse = ", ")

g_plot <- function (plotdat) {
  plotdat %>%
    mutate(Family = map_chr(dist, dist_name),
           g = map_chr(g, dist_name),
           Distribution = map_chr(distcat, .f = function(i) {
             distribution_levels %>% pluck(i)
           })) %>%
    ggplot(aes(
      x = g,
      y = ratio,
      shape = Distribution,
      colour = Family
    )) +
    geom_jitter(
      alpha = 0.6,
      size = 4,
      height = 0,
      width = 0.35
    ) +
    hrbrthemes::scale_color_ipsum() +
    labs(title = "Precision of approximated variance of the sample median",
         x = TeX("$g$"),
         y = TeX("$\\rho$")#,
         #caption = TeX("In this horizontally jittered plot, a small amount of horizontal random displacement is applied to the points, so that points with the same value of $\\rho$ are easily discerned.")
         ) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.key.width = unit(0.1, "cm"))
}


plotdat %>%
  dplyr::filter(g != "exp") %>%
  g_plot()

ggsave("../measureofcodeproof/figures/variance-subs.png")

# dev ---------------------------------------------------------------------
