library(rjd3highfreq)
a <- as.numeric(forecast::taylor)
mod <- fractionalAirlineDecomposition(
    y = a,
    period = 7,
    log = FALSE)

test_that("Decompose Model", {

    expect_equal("ArimaModel", mod$ucarima$model$name)
})

test_that("Decompose Model", {
    #number of components

    expect_equal(3, length(mod$ucarima$components))
})

reslt<-fractionalAirlineEstimation(a,c(7))

test_that("Estimate Model", {
    #ll
    expect_equal(-31687.65, reslt$likelihood$ll, tolerance=0.01)
})
