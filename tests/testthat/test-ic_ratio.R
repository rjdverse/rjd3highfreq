library(rjd3highfreq)
series <- rjd3toolkit::Retail$AllOtherGenMerchandiseStores

test_that("multiplication works", {
  expect_equal(ic_ratio(series), 4.038076, tolerance = 1e-3)
})

series_numeric <- as.numeric(series)

test_that("Errors", {
    expect_error(
        ic_ratio(series_numeric),
        "frequency cannot be NULL if series is not a ts"
    )
})
