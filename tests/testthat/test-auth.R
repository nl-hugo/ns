context("auth")

test_that("auth returns a request", {
  expect_is(auth(), "request")
})
