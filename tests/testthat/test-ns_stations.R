context("ns_stations")

res <- ns_stations()

test_that("it returns a dataframe", {
  expect_is(res, "data.frame")                # the result is a dataframe
})

test_that("it has the correct dimensions", {
  expect_true(dim(res)[1] > 600)              # more than 600 stations
  expect_equal(dim(res)[2], 9)                # 9 columns
})

test_that("it contains multiple countries", {
  expect_true(length(unique(res$land)) > 10)  # more than 10 different countries
})

test_that("all names have been filled", {
  expect_false(any(is.na(res$kort)))          # short name
  expect_false(any(is.na(res$middel)))        # middle name
  expect_false(any(is.na(res$lang)))          # long name
})

test_that("UT is Utrecht Centraal", {
  ut <- res[res$lang == "Utrecht Centraal", ]             # station by long name
  expect_equal(as.character(ut$code), "UT")               # code
  expect_equal(as.character(ut$type), "megastation")      # type
  expect_equal(as.character(ut$land), "NL")               # country
  expect_equal(as.character(ut$uiccode), "8400621")       # uic code
  expect_equal(as.character(ut$lat), "52.0888900756836")  # lat
  expect_equal(as.character(ut$lon), "5.11027765274048")  # lon
})
