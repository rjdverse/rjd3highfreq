series <- as.numeric(rjd3toolkit::Births$births)
result_x11 <- x11plus(series, 12)
out_x11 <- capture.output(print(result_x11))

result_stl <- stlplus(series, 12)
out_stl <- capture.output(print(result_stl))

result_mstl <- mstl(series,  period=c(7, 365.25))
out_mstl <- capture.output(print(result_mstl))

result_istl <- istl(series, period=c(7, 365.25))
out_istl <- capture.output(print(result_istl))

test_that("Print for class hf_decomposition", {
    expect_error(print(result_x11), NA)

    expect_equal("hf_decomposition", class(result_x11))

    expect_true(any(grepl("Decomposition:", out_x11)))

    expect_true(any(grepl("Parameters:", out_x11)))

    expect_error(print(result_stl), NA)

    expect_equal(class(out_stl), "character")

    expect_equal("hf_decomposition", class(result_stl))

    expect_true(any(grepl("Decomposition:", out_stl)))

    expect_true(any(grepl("Parameters:", out_stl)))

    expect_error(print(result_mstl), NA)

    expect_equal("hf_decomposition", class(result_mstl))

    expect_true(any(grepl("Decomposition:", out_stl)))

    expect_true(any(grepl("Parameters:", out_stl)))

    expect_error(print(result_istl), NA)

    expect_equal("hf_decomposition", class(result_istl))

    expect_true(any(grepl("Decomposition:", out_stl)))

    expect_true(any(grepl("Parameters:", out_stl)))

})


result_fractional <- fractional_airline_decomposition(series, 7)
out_fractional <- capture.output(print(result_fractional))

result_multi <- multi_airline_decomposition(series, c(7, 365.2425), log = TRUE, series_time = as.Date(rjd3toolkit::Births$date))
out_multi <- capture.output(print(result_multi))

result_estimation <- fractional_airline_estimation(series, c(7, 365.25), series_time = as.Date(rjd3toolkit::Births$date))
out_estimation <- capture.output(print(result_estimation))

test_that("Print for class hf_decomposition/hf_estimation", {
    expect_error(print(result_fractional), NA)

    expect_true(any(grepl("Decomposition:", out_fractional)))

    expect_true(any(grepl("Parameters:", out_fractional)))

    expect_true(any(grepl("series", out_fractional)))

    expect_true(any(grepl("sa", out_fractional)))

    expect_true(any(grepl("nfcasts", out_fractional)))

    expect_error(print(result_multi), NA)

    expect_true(any(grepl("Decomposition:", out_multi)))

    expect_true(any(grepl("Parameters:", out_multi)))

    expect_true(any(grepl("series", out_multi)))

    expect_true(any(grepl("sa", out_multi)))

    expect_error(print(result_estimation), NA)

    expect_equal("hf_estimation", class(result_estimation))

    expect_true(any(grepl("Fractional Airline estimation:", out_estimation)))

    expect_true(any(grepl("Regression results:", out_estimation)))

    expect_true(any(grepl("Likelihood:", out_estimation)))
})


