context("stations")

test_that("this is a list of stations", {
  res <- stations()
  expect_is(res, "ns")
  expect_equal(res$response$status_code, 200)
  expect_false(is.null(res$content))
})
