library(rjd3highfreq)

# STL Plus

series <- as.numeric(rjd3toolkit::Births$births)

result_stl <- stlplus(series, period=7, weight_function = "biweight")

test_that("Correct class", {
    expect_equal(class(result_stl), "hf_decomposition")
})

test_that("STL Decomposition", {
    expect_equal(result_stl$decomposition$series, series)

    expect_equal(tail(result_stl$decomposition$sa, 5), c(1693.961, 1711.621, 1804.000, 1680.407, 1642.082), tolerance = 1e-3)

    expect_equal(tail(result_stl$decomposition$t, 5), c(1684.859, 1684.056, 1683.722, 1683.547, 1683.368), tolerance = 1e-3)

    expect_equal(tail(result_stl$decomposition$s, 5), c(1.0714534, 0.9032374, 0.8519956, 1.0015433, 1.0614573), tolerance = 1e-3)

    expect_equal(tail(result_stl$decomposition$i, 5), c(1.0054018, 1.0163679, 1.0714359, 0.9981349, 0.9754742), tolerance = 1e-3)

    expect_equal(tail(result_stl$decomposition$fit, 5), c(1805.248, 1521.103, 1434.523, 1686.145, 1786.823), tolerance = 1e-3)
})

result_stl_uniform <- stlplus(series, period=7, weight_function = "uniform")
result_stl_triangular <- stlplus(series, period=7, weight_function = "triangular")
result_stl_epanechnikov <- stlplus(series, period=7, weight_function = "epanechnikov")
result_stl_tricube <- stlplus(series, period=7, weight_function = "tricube")
result_stl_triweight <- stlplus(series, period=7, weight_function = "triweight")

test_that("STL Weights", {
    expect_equal(tail(result_stl$decomposition$weights, 5), c(0.9856017, 0.8738441, 0.0000000, 0.9982521, 0.7286066), tolerance = 1e-3)

    expect_equal(tail(result_stl_uniform$decomposition$weights, 5), c(1, 1, 1, 1, 1), tolerance = 1e-3)

    expect_equal(tail(result_stl_triangular$decomposition$weights, 5), c(0.8862105, 0.7586113, 0.0000000, 0.9570694, 0.6044397), tolerance = 1e-3)

    expect_equal(tail(result_stl_epanechnikov$decomposition$weights, 5), c(0.9882745, 0.9718088, 0.4358858, 0.9871006, 0.8230106), tolerance = 1e-3)

    expect_equal(tail(result_stl_tricube$decomposition$weights, 5), c(0.9972811, 0.9521158, 0.0000000, 0.9998511, 0.8658846), tolerance = 1e-3)

    expect_equal(tail(result_stl_triweight$decomposition$weights, 5), c( 0.9830695, 0.7989497, 0.0000000, 0.9994256, 0.5592076), tolerance = 1e-3)
})

# MSTL

result_mstl <- mstl(series, period=c(7, 365.25))

test_that("Correct class", {
    expect_equal(class(result_mstl), "hf_decomposition")
})

test_that("MSTL Decomposition", {
    expect_equal(result_mstl$decomposition$series, series)

    expect_equal(tail(result_mstl$decomposition$sa, 5), c(1718.717, 1709.586, 1844.418, 1723.996, 1753.817), tolerance = 1e-3)

    expect_equal(tail(result_mstl$decomposition$t, 5), c(1681.573, 1681.413, 1681.254, 1681.095, 1680.935), tolerance = 1e-3)

    expect_equal(tail(result_mstl$decomposition$s7, 5), c(1.0786735, 0.9135772, 0.8655097, 0.9875026, 1.0337394), tolerance = 1e-3)

    expect_equal(tail(result_mstl$decomposition$s365, 5), c(0.9789991, 0.9898587, 0.9628146, 0.9885750, 0.9613954), tolerance = 1e-3)

    expect_equal(tail(result_mstl$decomposition$i, 5), c(1.022089, 1.016756, 1.097049, 1.025520, 1.043358), tolerance = 1e-3)

    expect_equal(tail(result_mstl$decomposition$fit, 5), c(1775.775, 1520.523, 1401.032, 1641.119, 1670.568), tolerance = 1e-3)
})

result_mstl_uniform <- mstl(series, period=c(7, 365.25), weight_function = "uniform")
result_mstl_triangular <- mstl(series, period=c(7, 365.25), weight_function = "triangular")
result_mstl_epanechnikov <- mstl(series, period=c(7, 365.25), weight_function = "epanechnikov")
result_mstl_tricube <- mstl(series, period=c(7, 365.25), weight_function = "tricube")
result_mstl_triweight <- mstl(series, period=c(7, 365.25), weight_function = "triweight")

test_that("MSTL Weights", {
    expect_equal(tail(result_mstl$decomposition$weights, 5), c(0.8266117, 0.9040705, 0.0000000, 0.7668208, 0.4181306), tolerance = 1e-3)

    expect_equal(tail(result_mstl_uniform$decomposition$weights, 5), c(1, 1, 1, 1, 1), tolerance = 1e-3)

    expect_equal(tail(result_mstl_triangular$decomposition$weights, 5), c(0.7054768, 0.8028269, 0.0000000, 0.6563464, 0.4518824), tolerance = 1e-3)

    expect_equal(tail(result_mstl_epanechnikov$decomposition$weights, 5), c(0.9545285, 0.9467797, 0.0000000, 0.8783231, 0.8245377), tolerance = 1e-3)

    expect_equal(tail(result_mstl_tricube$decomposition$weights, 5), c(0.9040294, 0.9757231, 0.0000000, 0.9147320, 0.7475029), tolerance = 1e-3)

    expect_equal(tail(result_mstl_triweight$decomposition$weights, 5), c(0.9786068, 0.7782254, 0.0000000, 0.2594082, 0.0000000), tolerance = 1e-3)
})

# ISTL

result_istl <- istl(series, period=c(7, 365.25))

test_that("Correct class", {
    expect_equal(class(result_istl), "hf_decomposition")
})

test_that("ISTL Decomposition", {
     expect_equal(result_istl$decomposition$series, series)

     expect_equal(tail(result_istl$decomposition$sa, 5), c(1713.195, 1704.315, 1881.872, 1715.326, 1725.177), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$t, 5), c(1681.610, 1681.442, 1681.274, 1681.106, 1680.938), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$s7, 5), c(1.0710879, 0.9030804, 0.8519342, 1.0017438, 1.0611686), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$s365, 5), c(0.9891106, 1.0044610, 0.9586891, 0.9794468, 0.9520930), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$i, 5), c(1.018782, 1.013603, 1.119313, 1.020355, 1.026318), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$fit, 5), c(1781.539, 1525.252, 1373.164, 1649.426, 1698.305), tolerance = 1e-3)

     expect_equal(tail(result_istl$decomposition$weights, 5), c(0.8980487, 0.9444879, 0.0000000, 0.8793468, 0.8026139), tolerance = 1e-3)
})

result_istl_uniform <- istl(series, period=c(7, 365.25), weight_function = "uniform")
result_istl_triangular <- istl(series, period=c(7, 365.25), weight_function = "triangular")
result_istl_epanechnikov <- istl(series, period=c(7, 365.25), weight_function = "epanechnikov")
result_istl_tricube <- istl(series, period=c(7, 365.25), weight_function = "tricube")
result_istl_triweight <- istl(series, period=c(7, 365.25), weight_function = "triweight")

test_that("ISTL Weights", {
    expect_equal(tail(result_istl$decomposition$weights, 5), c(0.8980487, 0.9444879, 0.0000000, 0.8793468, 0.8026139), tolerance = 1e-3)

    expect_equal(tail(result_istl_uniform$decomposition$weights, 5), c(1, 1, 1, 1, 1), tolerance = 1e-3)

    expect_equal(tail(result_istl_triangular$decomposition$weights, 5), c(0.7244594, 0.7299101, 0.0000000, 0.7536647, 0.6687697), tolerance = 1e-3)

    expect_equal(tail(result_istl_epanechnikov$decomposition$weights, 5), c(0.8784347, 0.8749603, 0.0000000, 0.9246858, 0.9029205), tolerance = 1e-3)

    expect_equal(tail(result_istl_tricube$decomposition$weights, 5), c(0.9436730, 0.9532853, 0.0000000, 0.9594920, 0.9118450), tolerance = 1e-3)

    expect_equal(tail(result_istl_triweight$decomposition$weights, 5), c(0.9489178, 0.9912631, 0.0000000, 0.8127051, 0.7049799), tolerance = 1e-3)
})

# Loess

q <- stlplus(
    series = rjd3toolkit::ABS$X0.2.09.10.M,
    period = 12
)

trend <- q$decomposition[, "t"]

smoothed_trend <- loess(
    series = trend,
    window = 121
)

test_that("Loess smoother", {
    expect_equal(tail(smoothed_trend, 5), c(1552.587, 1553.027, 1553.470, 1553.914, 1554.361), tolerance = 1e-3)

})

test_that("Loess Errors", {
    expect_error(loess(series = trend, window = 121, degree = 3),
        "Unsupported degree")

    expect_error(loess(series = trend, window = 121, jump = -1),
        "jump should be positive")
})
