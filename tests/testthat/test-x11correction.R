series <- rjd3toolkit::ABS$X0.2.09.10.M
result <- extreme_values_correction(series)

test_that("Extreme Value Correction", {
  expect_equal(unname(result[33,]), c(0.7943867, 213.7144210), tolerance = 1e-3)
})
