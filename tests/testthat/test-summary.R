series <- as.numeric(rjd3toolkit::Births$births)

result_x11 <- x11plus(series, 12)
result_stl <- stlplus(series, 12)
result_mstl <- mstl(series,  period=c(7, 365.25))
result_istl <- istl(series, period=c(7, 365.25))

result_fractional <- fractional_airline_decomposition(series, 7)
result_multi <- multi_airline_decomposition(series, c(7, 365.2425), log = TRUE, series_time = as.Date(rjd3toolkit::Births$date))
result_estimation <- fractional_airline_estimation(series, c(7, 365.25), series_time = as.Date(rjd3toolkit::Births$date))


test_that("Summary for class hf_decomposition", {
    expect_error(summary(result_x11), NA)
    expect_error(summary(result_stl), NA)
    expect_error(summary(result_mstl), NA)
    expect_error(summary(result_istl), NA)

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_x11)))))
    expect_true(any(grepl("Parameters:", capture.output(summary(result_x11)))))

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_stl)))))
    expect_true(any(grepl("Parameters:", capture.output(summary(result_stl)))))

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_mstl)))))
    expect_true(any(grepl("Parameters:", capture.output(summary(result_mstl)))))

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_istl)))))
    expect_true(any(grepl("Parameters:", capture.output(summary(result_istl)))))
})


test_that("Summary for class hf_decomposition/hf_estimation", {
    expect_error(summary(result_fractional), NA)

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_fractional)))))

    expect_true(any(grepl("Parameters:", capture.output(summary(result_fractional)))))

    expect_true(any(grepl("series", capture.output(summary(result_fractional)))))

    expect_true(any(grepl("sa", capture.output(summary(result_fractional)))))

    expect_true(any(grepl("nfcasts", capture.output(summary(result_fractional)))))

    expect_error(summary(result_multi), NA)

    expect_true(any(grepl("Decomposition:", capture.output(summary(result_multi)))))

    expect_true(any(grepl("Parameters:", capture.output(summary(result_multi)))))

    expect_true(any(grepl("series", capture.output(summary(result_multi)))))

    expect_true(any(grepl("sa", capture.output(summary(result_multi)))))

    expect_error(summary(result_estimation), NA)

    expect_equal("hf_estimation", class(result_estimation))

    expect_true(any(grepl("Fractional Airline estimation:", capture.output(summary(result_estimation)))))

    expect_true(any(grepl("Regression results:", capture.output(summary(result_estimation)))))

    expect_true(any(grepl("Likelihood:", capture.output(summary(result_estimation)))))
})


