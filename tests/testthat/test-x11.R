# Henderson filter

q <- x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
r <- henderson(q$decomposition$sa, 13)

test_that("Henderson filter", {
  expect_equal(tail(r, 5), c(1561.152, 1560.364, 1555.752, 1549.652, 1541.823), tolerance = 1e-3)
})

# X11 Plus

series <- rjd3toolkit::ABS$X0.2.09.10.M
result <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")

test_that("Correct class", {
    expect_equal(class(result), "hf_decomposition")
})

test_that("X11 Plus Decomposition", {
    expect_equal(result$decomposition$series, as.numeric(rjd3toolkit::ABS$X0.2.09.10.M))

    expect_equal(tail(result$decomposition$sa, 5), c(1606.790, 1567.279, 1562.725, 1504.045, 1551.671), tolerance = 1e-3)

    expect_equal(tail(result$decomposition$t, 5), c(1560.389, 1560.441, 1557.250, 1551.430, 1543.403), tolerance = 1e-3)

    expect_equal(tail(result$decomposition$s, 5), c(0.9476039, 0.9267019, 0.9964647, 0.9610747, 0.8398044), tolerance = 1e-3)

    expect_equal(tail(result$decomposition$i, 5), c(1.0297365, 1.0043816, 1.0035155, 0.9694573, 1.0053570), tolerance = 1e-3)
})

result_kernel_biweight <- x11plus(series, 12, trend_kernel = "BiWeight", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_triweight <- x11plus(series, 12, trend_kernel = "TriWeight", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_tricube <- x11plus(series, 12, trend_kernel = "TriCube", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_uniform <- x11plus(series, 12, trend_kernel = "Uniform", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_triangular <- x11plus(series, 12, trend_kernel = "Triangular", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_epanechnikov <- x11plus(series, 12, trend_kernel = "Epanechnikov", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")
result_kernel_trapezoidal <- x11plus(series, 12, trend_kernel = "Trapezoidal", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X5")

test_that("X11 Trend Kernel", {
    expect_equal(tail(result_kernel_biweight$decomposition$t, 5), c(1559.615, 1559.921, 1557.567, 1552.358, 1544.541), tolerance = 1e-3)

    expect_equal(tail(result_kernel_triweight$decomposition$t, 5), c(1564.037, 1562.669, 1556.171, 1546.780, 1537.492), tolerance = 1e-3)

    expect_equal(tail(result_kernel_tricube$decomposition$t, 5), c(1563.649, 1568.073, 1569.978, 1567.569, 1561.802), tolerance = 1e-3)

    expect_equal(tail(result_kernel_uniform$decomposition$t, 5), c(1562.742, 1566.444, 1569.371, 1577.015, 1574.192), tolerance = 1e-3)

    expect_equal(tail(result_kernel_triangular$decomposition$t, 5), c(1559.959, 1559.776, 1557.224, 1552.527, 1546.870), tolerance = 1e-3)

    expect_equal(tail(result_kernel_epanechnikov$decomposition$t, 5), c(1563.563, 1568.079, 1571.336, 1573.752, 1570.188), tolerance = 1e-3)

    expect_equal(tail(result_kernel_trapezoidal$decomposition$t, 5), c(1562.876, 1567.050, 1571.523, 1575.440, 1572.716), tolerance = 1e-3)
})

result_asymmetric_direct <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "Direct", seas_s0 = "S3X3", seas_s1 = "S3X5")

result_asymmetric_mmsre <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "MMSRE", seas_s0 = "S3X3", seas_s1 = "S3X5")

test_that("X11 Assymetric Trend", {
    expect_equal(tail(result_asymmetric_direct$decomposition$t, 5), c(1567.140, 1565.656, 1549.331, 1526.109, 1546.562), tolerance = 1e-3)

    expect_equal(tail(result_asymmetric_mmsre$decomposition$t, 5), c(1560.823, 1561.023, 1557.624, 1552.786, 1545.268), tolerance = 1e-3)
})


result_s0_1 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X1", seas_s1 = "S3X5")
result_s0_5 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X5", seas_s1 = "S3X5")
result_s0_9 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X9", seas_s1 = "S3X5")
result_s0_15 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X15", seas_s1 = "S3X5")

test_that("X11 Preliminary Seasonal Filters", {
    expect_equal(tail(result_s0_1$decomposition$s, 5), c(0.9473988, 0.9269407, 0.9980320, 0.9595642, 0.8387917), tolerance = 1e-3)

    expect_equal(tail(result_s0_5$decomposition$s, 5), c(0.9380964, 0.9289007, 0.9971108, 0.9641810, 0.8440493), tolerance = 1e-3)

    expect_equal(tail(result_s0_9$decomposition$s, 5), c(0.9393943, 0.9309517, 0.9984353, 0.9669027, 0.8462104), tolerance = 1e-3)

    expect_equal(tail(result_s0_15$decomposition$s, 5), c(0.9411191, 0.9315572, 0.9925202, 0.9698060, 0.8491271), tolerance = 1e-3)
})


result_s1_1 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X1")
result_s1_3 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X3")
result_s1_9 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X9")
result_s1_15 <- x11plus(series, 12, trend_kernel = "Henderson", trend_asymmetric = "CutAndNormalize", seas_s0 = "S3X3", seas_s1 = "S3X15")

test_that("X11 Final Seasonal Filters", {
    expect_equal(tail(result_s1_1$decomposition$s, 5), c(0.9329796, 0.9282774, 1.0072897, 0.9368142, 0.8406706), tolerance = 1e-3)

    expect_equal(tail(result_s1_3$decomposition$s, 5), c(0.9234024, 0.9276812, 1.0099365, 0.9481525, 0.8418333), tolerance = 1e-3)

    expect_equal(tail(result_s1_9$decomposition$s, 5), c(0.9376925, 0.9309718, 0.9916749, 0.9661768, 0.8445291), tolerance = 1e-3)

    expect_equal(tail(result_s1_15$decomposition$s, 5), c(0.9373008, 0.9326763, 0.9877689, 0.9685747, 0.8489028), tolerance = 1e-3)
})



test_that("User-defined series are returned", {
    series <- rjd3toolkit::ABS$X0.2.09.10.M

    result <- x11plus(
        series,
        period = 7,
        mul = TRUE,
        user_defined = c("d1", "d7")
    )

    # Result contains user_defined
    expect_true("user_defined" %in% names(result))

    user_defined <- result$user_defined

    # The series that we wanted are included
    expect_named(user_defined, c("d1", "d7"))
    expect_length(user_defined, 2)

    # Alle series are non-NULL
    expect_true(all(vapply(
        user_defined,
        function(x) {
            !is.null(x) &&
                is.numeric(x) &&
                length(x) > 0 &&
                all(is.finite(x))
        },
        logical(1)
    )))
})



