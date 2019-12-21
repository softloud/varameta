context("non-empty thing of expected type")

expect_neet <- function(fn_output, output_class) {
  expect_is(fn_output, output_class)
  expect_false(is.na(fn_output))
  expect_false(is.null(fn_output))
  expect_false(fn_output == Inf | fn_output == -Inf)
}

#
test_that({"g_exp"},
          expect_neet(g_exp(3, 5), "numeric")
          )
