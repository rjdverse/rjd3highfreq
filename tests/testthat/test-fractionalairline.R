library(rjd3highfreq)

#Fractional Airline Decomposition

a <- as.numeric(rjd3toolkit::Births$births)
mod_fractional <- fractional_airline_decomposition(
    a,
    period = 7,
    log = FALSE)

test_that("Correct class", {
    expect_equal(class(mod_fractional), "hf_decomposition")
})

test_that("Fractional Airline Decomposition", {
    expect_equal(mod_fractional$decomposition$series, a)

    expect_equal(tail(mod_fractional$decomposition$sa, 5), c(1708.454, 1700.294, 1769.898, 1690.691, 1637.262), tolerance = 1e-3)

    expect_equal(tail(mod_fractional$decomposition$s, 5), c(106.545813, -154.294362, -232.898146, -7.691153, 105.738040), tolerance = 1e-3)

    expect_equal(tail(mod_fractional$decomposition$t, 5), c(1633.480, 1641.583, 1646.644, 1648.355, 1648.167), tolerance = 1e-3)

    expect_equal(tail(mod_fractional$decomposition$i, 5), c(74.97412, 58.71116, 123.25417, 42.33632, -10.90471), tolerance = 1e-3)
})

test_that("Fractional Airline Model", {
    expect_equal(length(mod_fractional$ucarima$model), 5)

    expect_equal(mod_fractional$ucarima$model$var, 1)

    expect_equal(mod_fractional$ucarima$model$ma, c(-0.8212187, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, -0.97998870, 0.8047015), tolerance = 1e-3)

    expect_equal(mod_fractional$ucarima$model$delta, c(-1, 0, 0, 0, 0, 0, -1, 1))

    expect_equal(mod_fractional$ucarima$model$ar, numeric(0))

    expect_equal(mod_fractional$ucarima$model$name, "ArimaModel")
})

components <- as.list(mod_fractional$ucarima$components[[1]])

test_that("Fractional Airline Components", {
    expect_equal(length(mod_fractional$ucarima$components), 3)

    expect_equal(components[[1]], "ArimaModel")

    expect_equal(components[[3]], c(-2, 1))

    expect_equal(components[[4]], c(0.002898053, -0.997101947), tolerance = 1e-3)

    expect_equal(components[[5]], 0.00785471, tolerance = 1e-3)
})

test_that("Fractional Airline Estimation", {
    expect_equal(mod_fractional$estimation$parameters, c(0.8212187, 0.9798870), tolerance = 1e-3)

    expect_equal(mod_fractional$estimation$score, c(0.3096883, 4.4406122), tolerance = 1e-3)

    expect_equal(mod_fractional$estimation$covariance, matrix(c(2.040366e-06, 3.685811e-09, 3.685811e-09, 1.073229e-06), nrow = 2, byrow = TRUE), tolerance = 1e-3)

    expect_equal(mod_fractional$estimation$periods, 7)

    expect_equal(mod_fractional$estimation$log, FALSE)
})

test_that("Fractional Airline Likelihood", {
    expect_equal(mod_fractional$likelihood$ll, -124179, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$ssq, 185329193, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$nobs, 20820)

    expect_equal(mod_fractional$likelihood$neffectiveobs, 20812)

    expect_equal(mod_fractional$likelihood$nparams, 3)

    expect_equal(mod_fractional$likelihood$df, 20809, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$aic, 248364, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$aicc, 248364, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$bic, 248387.8, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$bic2, 11.93483, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$bicc, 9.095315, tolerance = 1e-3)

    expect_equal(mod_fractional$likelihood$hannanquinn, 248371.7, tolerance = 1e-3)
})

test_that("Errors Fractional Airline Decomposition", {
    expect_error(
        fractional_airline_decomposition(series="series"),
        "series must be numeric.")

    expect_error(
        fractional_airline_decomposition(a, period = c(7, 365.2425)),
        "period must be a numeric value of length 1.")

    expect_error(
        fractional_airline_decomposition(a, period = integer(365.2425)),
        "period must be a numeric value of length 1.")

    expect_error(
        fractional_airline_decomposition(a, period = (7), sn = c(TRUE, TRUE)),
        "sn must be a logical value of length 1.")

    expect_error(
        fractional_airline_decomposition(a, period = (7), sn = "TRUE"),
        "sn must be a logical value of length 1.")
})

# Multi Airline Decomposition

mod_multi <- multi_airline_decomposition(a, c(7, 365.2425), log = TRUE, series_time = as.Date(rjd3toolkit::Births$date))

test_that("Correct class", {
    expect_equal(class(mod_multi), "hf_decomposition")
})

test_that("Multi Airline Decomposition", {
    expect_equal(mod_multi$decomposition$series, a)

    expect_equal(tail(mod_multi$decomposition$sa, 5), c(1691.703, 1695.203, 1699.878, 1702.491, 1698.851), tolerance = 1e-3)
})

test_that("Multi Airline Model", {
    expect_equal(length(mod_multi$ucarima$model), 5)

    expect_equal(mod_multi$ucarima$model$var, 4.351876, tolerance = 1e-3)

    expect_equal(tail(mod_multi$ucarima$model$ma, 5), c(-0.002085676, -0.016631049, 0.655129344, 0.236234348, 0.007959319), tolerance = 1e-3)

    expect_equal(mod_multi$ucarima$model$delta, c(-1, 0, 0, 0, 0, 0, -1, 1))

    expect_equal(mod_multi$ucarima$model$ar[365], 0.2425)

    expect_equal(mod_multi$ucarima$model$name, "ArimaModel")
})

components <- as.list(mod_multi$ucarima$components[[1]])

test_that("Multi Airline Components", {
    expect_equal(length(mod_multi$ucarima$components), 4)

    expect_equal(components[[1]], "ArimaModel")

    expect_equal(components[[3]], c(-2, 1))

    expect_equal(components[[4]], c(0, -1))

    expect_equal(components[[5]], 0.0001277202, tolerance = 1e-3)
})

test_that("Multi Airline Estimation", {
    expect_equal(mod_multi$estimation$parameters, c(-0.2091582, 0.8540306, 0.7996361), tolerance = 1e-3)

    expect_equal(mod_multi$estimation$score, c(-0.5919189, -2.4275713, 0.2576903), tolerance = 1e-3)

    expect_equal(mod_multi$estimation$covariance, matrix(c(9.528337e-07, -3.126453e-09, 1.323750e-10, -3.126453e-09, 9.056693e-07, -2.877548e-09, 1.323750e-10, -2.877548e-09, 9.469313e-07), nrow = 3, byrow = TRUE), tolerance = 1e-3)

    expect_equal(mod_multi$estimation$period, c(7.0000, 365.2425))

    expect_equal(mod_multi$estimation$log, TRUE)
})

test_that("Multi Airline Likelihood", {
    expect_equal(mod_multi$likelihood$ll, -122218.5, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$ssq, 151912831, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$nobs, 20820)

    expect_equal(mod_multi$likelihood$neffectiveobs, 20812)

    expect_equal(mod_multi$likelihood$nparams, 4)

    expect_equal(mod_multi$likelihood$df, 20808, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$aic, 244445.1, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$aicc, 244445.1, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$bic, 244476.8, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$bic2, 11.74692, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$bicc, 8.896966, tolerance = 1e-3)

    expect_equal(mod_multi$likelihood$hannanquinn, 244455.5, tolerance = 1e-3)
})

test_that("Errors Multi Airline Decomposition", {
    expect_error(
        multi_airline_decomposition(series="series", period = c(7, 365.2425)),
        "series must be numeric.")
})


# Fractional Airline Estimation

mod_estimation <- fractional_airline_estimation(
    a,
    c(7, 365.25),
    outliers = c("AO", "LS", "WO"),
    critical_value = 8,
    series_time = as.Date(rjd3toolkit::Births$date)
)

test_that("Correct class", {
    expect_equal(class(mod_estimation), "hf_estimation")
})

test_that("Fractional Airline Estimation Model", {
    expect_equal(mod_estimation$model$variables, c("LS.2021-02-17", "LS.1970-10-23", "LS.1975-08-10", "AO.1996-05-16", "AO.1998-05-21"))

    expect_equal(mod_estimation$model$b, c(178.9783, 178.6364, -157.1903, -639.8608, -627.3145), tolerance = 1e-3)

    expect_equal(mod_estimation$model$bcov, matrix(c(3.352128e+02, 6.892778e-05, 7.466104e-05, 0.00997988, 1.413856e-02, 6.892778e-05,  3.404640e+02, 4.158601e+00, -0.00142744, -1.100608e-03,  7.466104e-05, 4.158601e+00, 3.281138e+02, -0.02283011, -1.578104e-02, 9.979880e-03, -1.427440e-03, -2.283011e-02, 6136.64295374, -1.363668e+00, 1.413856e-02, -1.100608e-03, -1.578104e-02, -1.36366807, 6.136643e+03), nrow = 5, byrow = TRUE), tolerance = 1e-3)
})

test_that("Fractional Airline Estimation Decomposition", {
    expect_equal(mod_estimation$estimation$series_time, rjd3toolkit::Births$date)

    expect_equal(mod_estimation$decomposition$series, a)

    expect_equal(tail(mod_estimation$decomposition$linearized), c(1650, 1815, 1546, 1537, 1683, 1743))

    expect_equal(tail(mod_estimation$decomposition$residuals), c(2.698402, 70.016301, 17.318193, 119.589764, 77.363308, 132.971950), tolerance = 1e-3)
})

test_that("Fractional Airline Estimation Estimation", {
    expect_equal(mod_estimation$estimation$parameters, c(-0.2061241, 0.8681462, 0.7937781), tolerance = 1e-3)

    expect_equal(mod_estimation$estimation$score, c(-1.117421, 4.476537, 1.452036), tolerance = 1e-3)

    expect_equal(mod_estimation$estimation$covariance, matrix(c(4.288723e-05, -7.114980e-06, 1.509830e-06, -7.114980e-06, 2.374146e-05, -5.072679e-06, 1.509830e-06, -5.072679e-06, 3.003266e-05), nrow = 3, byrow = TRUE), tolerance = 1e-3)

    expect_equal(mod_estimation$estimation$period, c(7.00, 365.25))

    expect_equal(mod_estimation$estimation$log, FALSE)
})

test_that("Fractional Airline Estimation Likelihood", {
    expect_equal(mod_estimation$likelihood$ll, -122016.6, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$ssq, 148979893, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$nobs, 20820)

    expect_equal(mod_estimation$likelihood$neffectiveobs, 20812)

    expect_equal(mod_estimation$likelihood$nparams, 9)

    expect_equal(mod_estimation$likelihood$df, 20803, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$aic, 244051.2, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$aicc, 244051.2, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$bic, 244122.7, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$bic2, 11.7299, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$bicc, 8.879859, tolerance = 1e-3)

    expect_equal(mod_estimation$likelihood$hannanquinn, 244074.5, tolerance = 1e-3)
})

test_that("Errors Fractional Airline Estimation", {
    expect_error(
        fractional_airline_estimation(series="series"),
        "series must be numeric."
        )

    expect_error(
        fractional_airline_estimation(series = a, c(7, 365.25), series_time = as.Date(rjd3toolkit::Births$date), outliers = c("AO", "LS", "WO"), critical_value = TRUE),
        "critical_value must be a numeric value of length 1."
        )

    expect_error(
        fractional_airline_estimation(series = a, c(7, 365.25), series_time = as.Date(rjd3toolkit::Births$date), outliers = c("AO", "LS", "WO"), critical_value = c(4, 6)),
        "critical_value must be a numeric value of length 1."
        )

    expect_error(
        fractional_airline_estimation(series = a, c(7, 365.25), precision = TRUE),
        "precision must be a numeric value of length 1."
        )

    expect_error(
        fractional_airline_estimation(series = a, c(7, 365.25), precision = c(1e-10, 1e-12)),
        "precision must be a numeric value of length 1."
        )
})

