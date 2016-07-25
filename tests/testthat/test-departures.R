context("departures")

test_that("this is a list of departures", {
  res <- departures("MRN")
  expect_is(res, "ns")
  expect_equal(res$response$status_code, 200)
  expect_false(is.null(res$content))
})
