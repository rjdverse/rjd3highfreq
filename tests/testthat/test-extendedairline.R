library(rjd3highfreq)

# Loglevel

a <- rjd3toolkit::Retail$BookStores
result <- loglevel(a)

test_that("AICc of the model in levels or with logs", {
    expect_equal(unname(result[1]), 2463.671, tolerance = 1e-3)

    expect_equal(unname(result[2]), 2380.876, tolerance = 1e-3)
})

# Estimate Extended Airline

b <- rjd3toolkit::Births[,2]
result <- .estimate_extended_airline(b, period= c(7, 365.25))

test_that("Correct class", {
    expect_equal(class(result), "extended_airline")
})

test_that("Estimation", {
    expect_equal(result$data$series, as.numeric(b))
})

test_that("Regarima", {
    expect_equal(result$regarima$getXCount(), 0)

    expect_equal(result$regarima$getObservationsCount(), 20820)

    expect_equal(result$regarima$getVariablesCount(), 0)

    expect_equal(result$regarima$getMissingValuesCount(), 0)

    expect_equal(result$regarima$getActualObservationsCount(), 20820)

    expect_equal(result$regarima$isMean(), FALSE)

    expect_equal(result$regarima$missing(), integer(0))
})

test_that("Errors", {
    expect_error(
        .estimate_extended_airline(b),
        "`period` must be a non-empty numeric vector."
    )

    expect_error(
        .estimate_extended_airline(series = c()),
        "`series` must be a non-empty numeric vector.")

    expect_error(
        .estimate_extended_airline(period = (c(7, 365.25))),
        "`series` must be a non-empty numeric vector.")
})

