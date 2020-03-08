context("effect standard error estimator")

library(varameta)


# reproducibility ---------------------------------------------------------

# set.seed() only after I'm done with early dev stage


# fuzz testing samples ----------------------------------------------------

big <- runif(1, 20, 100)
small <- runif(1, 0.1, 0.9)
n <- sample(seq(5, 100), 1) %>% as.numeric()


# test inputs and defaults ------------------------------------------------

test_that("effect_se works for various inputs", {
  # check output is a number
  expect_is(effect_se(
    centre = big,
    spread = small,
    n = n
  ), "numeric")
  # check output is length 1
  expect_true(length(effect_se(
    centre = big,
    spread = small,
    n = n
  )) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = big,
    spread = small,
    n = n
  ) > 0)
  # check output is a number
  expect_is(effect_se(
    centre = 5,
    spread = 1,
    n = 10
  ), "numeric")
  # check output is length 1
  expect_true(length(effect_se(
    centre = 67,
    spread = 0.3,
    n = 20
  )) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = 45,
    spread = 2,
    n = 30
  ) > 0)
})

# means -------------------------------------------------------------------


test_that("mean return expected spread", {
  expect_equal(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "var"
  ),
  sqrt(small / n))
  expect_equal(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "sd"
  ),
  small / sqrt(n))
  expect_equal(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "se"
  ),
  small)

})


test_that("variance outputs postive number", {
  # check output is a number
  expect_is(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "var"
  ),
  "numeric")
  # check output is length 1
  expect_true(length(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "var"
  )) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "var"
  ) > 0)
})


test_that("sd outputs postive number", {
  # check output is a number
  expect_is(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "sd"
  ),
  "numeric")
  # check output is length 1
  expect_true(length(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "sd"
  )) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = big,
    spread = small,
    n = n,
    spread_type = "sd"
  ) > 0)
  # check output is a number
  expect_is(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "mean",
      spread_type = "sd"
    ),
    "numeric"
  )
  # check output is length 1
  expect_true(length(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "mean",
      spread_type = "sd"
    )
  ) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = big,
    spread = small,
    n = n,
    centre_type = "mean",
    spread_type = "sd"
  ) > 0)
})


test_that("seiance outputs postive number", {
  # check output is a number
  expect_is(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "mean",
      spread_type = "se"
    ),
    "numeric"
  )
  # check output is length 1
  expect_true(length(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "mean",
      spread_type = "se"
    )
  ) == 1)
  # checkout output is positive
  expect_true(effect_se(
    centre = big,
    spread = small,
    n = n,
    centre_type = "mean",
    spread_type = "se"
  ) > 0)
})



# medians -----------------------------------------------------------------


test_that("median se is a single postive number", {
  # check output is a number
  expect_is(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "iqr"
    ),
    "numeric"
  )
  # check output is length 1
  expect_true(length(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "iqr"
    )
  ) == 1)
  # checkout output is positive
  expect_true(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "iqr"
    ) > 0
  )
  # check output is a number
  expect_is(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "range"
    ),
    "numeric"
  )
  # check output is length 1
  expect_true(length(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "range"
    )
  ) == 1)
  # checkout output is positive
  expect_true(
    effect_se(
      centre = big,
      spread = small,
      n = n,
      centre_type = "median",
      spread_type = "range"
    ) > 0
  )
})
