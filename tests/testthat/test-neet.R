context("non-empty thing of expected type")

library(neet)

test_that("g_*", {
  expect_neet(g_exp(3, 5), "numeric")
  expect_neet(g_cauchy(3, 5, 2), "numeric")
  expect_neet(g_lnorm(3, 5, 2), "numeric")
  expect_neet(g_norm(3, 5, 2), "numeric")
})

test_that("effect_se", {
  # testing sample
  a_sample <- rexp(100, 3)

  expect_neet(
    effect_se(
      centre = mean(a_sample),
      spread = sd(a_sample),
      n = length(a_sample),
      centre_type = "mean",
      spread_type = "sd"
    ), "numeric"
  )

  expect_neet(
    effect_se(
      centre = mean(a_sample),
      spread = var(a_sample),
      n = length(a_sample),
      centre_type = "mean",
      spread_type = "var"
    ), "numeric"
  )

  expect_neet(
    effect_se(
      centre = median(a_sample),
      spread = IQR(a_sample),
      n = length(a_sample),
      centre_type = "median",
      spread_type = "iqr"
    ), "numeric"
  )

  expect_neet(
    effect_se(
      centre = median(a_sample),
      spread = diff(range(a_sample)),
      n = length(a_sample),
      centre_type = "median",
      spread_type = "range"
    ), "numeric"
  )

})

test_that("other estimators", {
  # testing sample
  a_sample <- rexp(100, 3)

  expect_neet(bland_mean(
    min(a_sample),
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75),
    max(a_sample)
  ), "numeric")

  expect_neet(bland_se(
    min(a_sample),
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75),
    max(a_sample),
    length(a_sample)
  ), "numeric")

  expect_neet(hozo_mean(min(a_sample),
                        median(a_sample),
                        max(a_sample)), "numeric")

  expect_neet(hozo_se(
    min(a_sample),
    median(a_sample),
    max(a_sample),
    length(a_sample)
  ), "numeric")

  expect_neet(wan_mean_C1(min(a_sample),
                          median(a_sample),
                          max(a_sample)), "numeric")

  expect_neet(wan_se_C1(min(a_sample),
                        max(a_sample),
                        length(a_sample)), "numeric")

  expect_neet(wan_mean_C2(
    min(a_sample),
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75),
    max(a_sample)
  ), "numeric")

  expect_neet(wan_se_C2(
    min(a_sample),
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75),
    max(a_sample),
    length(a_sample)
  ), "numeric")

  expect_neet(wan_mean_C3(
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75)
  ), "numeric")

  expect_neet(wan_se_C3(
    quantile(a_sample, 0.25),
    median(a_sample),
    quantile(a_sample, 0.75),
    length(a_sample)
  ), "numeric")

})
