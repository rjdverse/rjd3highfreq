# Linearize the series with a fractional airline model

Linearize the series with a fractional airline model

## Usage

``` r
fractionalAirlineEstimation(
  y,
  periods,
  x = NULL,
  ndiff = 2,
  ar = FALSE,
  outliers = NULL,
  criticalValue = 6,
  precision = 1e-12,
  approximateHessian = FALSE,
  nfcasts = 0,
  log = FALSE,
  y_time = NULL
)
```

## Arguments

- y:

  input time series.

- periods:

  vector of periods values of the seasonal component, any positive real
  numbers.

- x:

  matrix of user-defined regression variables (see rjd3toolkit for
  building calendar regressors).

- outliers:

  type of outliers sub vector of c("AO","LS","WO")

- criticalValue:

  Critical value for automatic outlier detection

- precision:

  Precision of the likelihood

- approximateHessian:

  Compute approximate hessian (based on the optimizing procedure)

- nfcasts:

  Number of forecasts

- log:

  a logical

- y_time:

  vector of times at which \`y\` is indexed
