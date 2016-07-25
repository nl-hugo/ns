context("ns")

test_that("this is a faulty path", {
  expect_error(ns("this-is-a-faulty-path"),
                  "NS API request failed.*002:The requested webservice is not found")
})

test_that("this is a list of stations", {
  path <- "ns-api-stations-v2"
  res <- ns(path)
  expect_is(res, "ns")
  expect_equal(res$response$status_code, 200)
  expect_equal(res$path, path)
  expect_false(is.null(res$content))
})
