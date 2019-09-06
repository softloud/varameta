#' varameta estimator trial
#'
#' @param n sample size
#' @inheritParams simulate_study_k
#'
#' @export

varameta_trial <- function(n = 15, ...) {
  simulate_study_k(n, n, ...) %>%
    tibble::as_tibble(
    ) %>%
    dplyr::mutate(
      # calculate standard error of each median
      c_se = effect_se(
        n = n_c,
        centre = median_c,
        spread = iqr_c,
        centre_type = "median",
        spread_type = "iqr"
      ),
      i_se = effect_se(
        n = n_i,
        centre = median_i,
        spread = iqr_i,
        centre_type = "median",
        spread_type = "iqr"
      ),
      median_diff = list(list(
        centre = median_i - median_c,
        spread = sqrt(c_se ^ 2 + i_se ^ 2)
      )),
      # calculate combined effect
      m = list(list(
        centre = median_c,
        spread = c_se
      )),
      lr = list(list(
        centre = log(median_i / median_c),
        spread = sqrt(c_se / median_c ^ 2 + i_se / median_i ^ 2)
      ))
    ) %>%
    tidyr::gather(key = "effect_type",
           value = "effects",
           m, median_diff, lr) %>%
    dplyr::mutate(
      effect = purrr::map_dbl(effects, "centre"),
      effect_se = purrr::map_dbl(effects, "spread")
    ) %>%
    # dplyr::select(effect_type, effect, effect_se, n) %>%
    dplyr::mutate(ci_lb = effect - qnorm(0.975) * effect_se,
           ci_ub = effect - qnorm(0.975) * effect_se)

}
