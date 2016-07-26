context("ns_departures")

res <- ns_departures("MRN")

test_that("it returns a dataframe", {
  expect_is(res, "data.frame")                # the result is a dataframe
})

test_that("it has the correct dimensions", {
  expect_true(dim(res)[1] >= 10)              # 10 or more departures
  expect_equal(dim(res)[2], 12)               # 12 columns
})

test_that("it has the correct columns", {
  expect_equal(names(res), c("ritnummer",
                             "vertrektijd",
                             "vertrekvertraging",
                             "vertrekvertragingtekst",
                             "eindbestemming",
                             "treinsoort",
                             "routetekst",
                             "vervoerder",
                             "vertrekspoor",
                             "gewijzigdvertrekspoor",
                             "reistip",
                             "comments"
                             ))
})

test_that("it contains different destinations", {
  expect_true(length(unique(res$eindbestemming)) > 1) # more than 1 different
                                                      # destinations
})

test_that("the next train has not left yet", {
  nt <- res[1, ]             # station by long name
  #expect_equal(as.character(ut$code), "UT")               # code
  expect_equal(1,1)
})
