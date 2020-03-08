context("estimators")

library(varameta)

# testing sample ----------------------------------------------------------

# test numbers

big_negative_number <- runif(3,-100,-1.1)
small_negative_number <- runif(3,-0.9,-0.1)
big_positive_number <- runif(3, 1.1, 100) %>% round()
small_positive_number <- runif(3, 0.1, 0.9)

# generate testing sample for different sample sizes
ts <-
  c(
    sample(seq(5, 15), 1) %>% as.numeric(),
    # some estimators are case-wise defined
    sample(seq(16, 70), 1) %>% as.numeric(),
    sample(seq(70, 100), 1) %>% as.numeric()
  ) %>%
  purrr::map(runif) %>% # sample some uninteresting data
  {
    # calculate summary statistics
    tibble::tibble(
      min = purrr::map_dbl(., min),
      max = purrr::map_dbl(., max),
      first_q = purrr::map_dbl(., quantile, probs = 0.25),
      third_q = purrr::map_dbl(., quantile, probs = 0.75),
      iqr = purrr::map_dbl(., IQR),
      mean = purrr::map_dbl(., mean),
      median = purrr::map_dbl(., median),
      sd = purrr::map_dbl(., sd),
      n = purrr::map_dbl(., length)
    )
  } %>% # calculate estimators and worked examples
  dplyr::mutate(
    # effect_se_iqr = purrr::pmap_dbl(list(centre = median, spread = iqr, n = n),
    #                          .f = varameta::effect_se,
    #                          centre_type = "median",
    #                          spread_type = "iqr"),
    hozo_mean = purrr::pmap_dbl(list(
      a = min,
      m = median,
      b = max
    ), hozo_mean),
    hozo_mean_wkd = (min + 2 * median + max) / 4,
    hozo_se = purrr::pmap_dbl(list(
      a = min,
      m = median,
      b = max,
      n = n
    ), hozo_se),
    hozo_se_wkd_case1 = sqrt(((max - min) ^ 2 +
                                (min - 2 * median + max) ^ 2 / 4) / 12) /
      sqrt(n),
    hozo_se_wkd_case2 = ((max - min) / 4) / sqrt(n),
    hozo_se_wkd_case3 = ((max - min) / 6) / sqrt(n),
    bland_mean = purrr::pmap_dbl(
      list(
        a =  min,
        q_1 = first_q,
        m = median,
        q_3 = third_q,
        b = max
      ),
      bland_mean
    ),
    bland_mean_wkd = (min + 2 * first_q + 2 * median + 2 * third_q + max) / 8,
    bland_se = purrr::pmap_dbl(
      list(
        a =  min,
        q_1 = first_q,
        m = median,
        q_3 = third_q,
        b = max,
        n = n
      ),
      bland_se
    ),
    bland_se_wkd = sqrt((
      1 / 16 * (min ^ 2 +
                  2 * first_q ^ 2 +
                  2 * median ^ 2 +
                  2 * third_q ^ 2 +
                  max ^ 2) +
        1 / 8 * (min * first_q +
                   first_q * median +
                   median * third_q +
                   third_q * max) -
        1 / 64 * (min +
                    2 * first_q +
                    2 * median +
                    2 * third_q +
                    max) ^ 2
    ) / n),
    wan_mean_C1 = purrr::pmap_dbl(list(
      a = min,
      m = median,
      b = max
    ),
    wan_mean_C1),
    wan_mean_C1_wkd = (min + 2 * median + max) / 4,
    wan_se_C1 = purrr::pmap_dbl(list(
      a = min,
      b = max,
      n = n
    ),
    wan_se_C1),
    wan_se_C1_wkd = purrr::pmap_dbl(
      list(a = min, b = max, n = n),
      .f =
        function(a, b, n) {
          ((b - a) / (2 * qnorm((n - 0.375) / (n + 0.25)))) / sqrt(n)
        }
    ),
    wan_mean_C2 = purrr::pmap_dbl(
      list(
        a = min,
        q_1 = first_q,
        m = median,
        q_3 = third_q,
        b = max
      ),
      wan_mean_C2
    ),
    wan_mean_C2_wkd = (min + 2 * first_q + 2 * median + 2 * third_q + max) / 8,
    wan_se_C2 = purrr::pmap_dbl(
      list(
        a = min,
        q_1 = first_q,
        m = median,
        q_3 = third_q,
        b = max,
        n = n
      ),
      wan_se_C2
    ),
    wan_se_C2_wkd = ((max - min) / (4 * qnorm((
      n - 0.375
    ) / (
      n + 0.25
    ))) +
      (third_q - first_q) / (4 * qnorm((0.75 * n - 0.125) / (n + 0.25)
      ))) / sqrt(n),
    wan_mean_C3 = purrr::pmap_dbl(list(
      q_1 = first_q, m = median, q_3 = third_q
    ),
    wan_mean_C3),
    wan_mean_C3_wkd = (first_q + median + third_q) / 3,
    wan_se_C3 = purrr::pmap_dbl(list(
      q_1 = first_q,
      m = median,
      q_3 = third_q,
      n = n
    ),
    wan_se_C3),
    wan_se_C3_wkd = ((third_q - first_q) /
                       (2 * qnorm((0.75 * n - 0.125) / (n + 0.25)
                       ))) / sqrt(n)
  )


# tests -------------------------------------------------------------------



## mean estimators --------------------------------------------------------

# hozo estimators
test_that("hozo estimators", {
  # mean works as expected
  expect_equal(ts$hozo_mean, ts$hozo_mean_wkd)
  # se works as expected for n <= 15
  expect_equal(ts$hozo_se[[1]], ts$hozo_se_wkd_case1[[1]])
  # se works as expected for 15 < n <= 79
  expect_equal(ts$hozo_se[[2]], ts$hozo_se_wkd_case2[[2]])
  # se works as expected for n > 70
  expect_equal(ts$hozo_se[[3]], ts$hozo_se_wkd_case3[[3]])
})

# bland estimators
test_that("bland estimators", {
  # mean works as expected
  expect_equal(ts$bland_mean, ts$bland_mean_wkd)
  # se works as expected
  expect_equal(ts$bland_se, ts$bland_se_wkd)
})

# wan C1 estimators
test_that("wan C1 estimators", {
  # mean works as expected
  expect_equal(ts$wan_mean_C1, ts$wan_mean_C1_wkd)
  # se works as expected
  expect_equal(ts$wan_se_C1, ts$wan_se_C1_wkd)
  # hozo mean and wan C1 are the same
  expect_equal(ts$wan_mean_C1, ts$hozo_mean)
})

# wan C2 estimators
test_that("wan C2 estimators", {
  # mean works as expected
  expect_equal(ts$wan_mean_C2, ts$wan_mean_C2_wkd)
  # se works as expected
  expect_equal(ts$wan_se_C2, ts$wan_se_C2_wkd)
})

# wan C3 estimators
test_that("wan C3 estimators", {
  # mean works as expected
  expect_equal(ts$wan_mean_C3, ts$wan_mean_C3_wkd)
  # se works as expected
  expect_equal(ts$wan_se_C3, ts$wan_se_C3_wkd)
})


## varameta estimators ----------------------------------------------------


test_that("g_norm", {
  expect_true(varameta:::g_norm(100, 50, 2) > 0)
  expect_is(varameta:::g_norm(100, 50, 2), "numeric")
  expect_is(varameta:::g_norm(30, 5, 2), "numeric")
  expect_equal(varameta:::g_norm(1, 50, 3), 1 / (2 * dnorm(50, 50, 3 / (2 *
                                                                          qnorm(
                                                                            0.75
                                                                          )))))
})

test_that("g_lnorm", {
  # test numeric values
  expect_is(varameta:::g_lnorm(50, 5, 2, "iqr"), "numeric")
  expect_is(
    varameta:::g_lnorm(
      big_positive_number[[1]],
      small_positive_number[[1]],
      big_positive_number[[2]]
    ),
    "numeric"
  )
  expect_equal(varameta:::g_lnorm(10, 4, 0.3),
               1 / (2 * sqrt(10) * dlnorm(4, log(4), 1 / qnorm(3 / 4) * log((0.3 * exp(-log(4)) + sqrt(0.3 ^
                                                                                                         2 * exp(-2 * log(
                                                                                                           4
                                                                                                         )) + 4)) / 2
               ))))
  expect_true(varameta:::g_lnorm(50, 5, 2, "iqr") > 0)
})


test_that("g_exp", {
  expect_is(varameta:::g_exp(50, 3), "numeric")
  expect_is(varameta:::g_exp(abs(big_positive_number[[1]]), abs(big_positive_number[[2]])), 'numeric')
  expect_equal(varameta:::g_exp(5, 10), 1 / (2 * sqrt(5) * dexp(10, rate = log(2) / 10)))
  expect_equal(
    varameta:::g_exp(big_positive_number[[1]], big_positive_number[[2]]),
    1 / (
      2 * sqrt(big_positive_number[[1]]) * dexp(big_positive_number[[2]], rate = log(2) / big_positive_number[[2]])
    )
  )

})

test_that("g_cauchy", {
  expect_is(varameta:::g_cauchy(50, 5, 2), "numeric")
  expect_equal(varameta:::g_cauchy(50, 6, 2), 1 / (2 * sqrt(50) * dcauchy(5, 5, 2 / 2)))
  expect_equal(
    varameta:::g_cauchy(
      big_positive_number[[1]],
      big_positive_number[[2]],
      big_positive_number[[3]]
    ),
    1 / (
      2 * sqrt(big_positive_number[[1]]) * dcauchy(
        big_positive_number[[2]],
        big_positive_number[[2]],
        big_positive_number[[3]] / 2
      )
    )
  )
})
