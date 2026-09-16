# Linearize a time series using a fractional airline model

This function estimates a (fractional) airline RegARIMA model and
returns the linearized series together with regression effects, outlier
components, estimation results and likelihood diagnostics. It is
typically used as a preprocessing step prior to AMB or UCM-based
decompositions.

## Usage

``` r
fractional_airline_estimation(
  series,
  period,
  xreg = NULL,
  ndiff = 2,
  ar = FALSE,
  mean = FALSE,
  outliers = NULL,
  critical_value = 6,
  precision = 1e-12,
  deps = 1e-04,
  approximate_hessian = FALSE,
  nfcasts = 0,
  log = FALSE,
  series_time = NULL
)
```

## Arguments

- series:

  input time series.

- period:

  numeric vector of seasonal periods. Each value must be a positive real
  number (e.g. 7 for weekly, 365.2425 for annual seasonality).

- xreg:

  optional matrix of user-defined regression variables (e.g. calendar
  regressors built using `rjd3toolkit`).

- ndiff:

  integer specifying the number of regular differences. Default is 2.

- ar:

  logical. If TRUE, an autoregressive component is included in the
  model. Default is FALSE.

- mean:

  logical. If TRUE, a mean component is included in the model. Default
  is FALSE.

- outliers:

  character vector specifying the types of outliers to detect. Possible
  values include `"AO"`, `"LS"` and `"WO"`. Default is `NULL` (no
  automatic outlier detection).

- critical_value:

  numeric. Critical value for automatic outlier detection. Larger values
  imply more conservative detection. Default is 6.

- precision:

  numeric. Precision of the likelihood optimization. Default is `1e-12`.

- deps:

  step in the computation of the numerical derivatives, used in the
  optimisation routine. Default:1e-4

- approximate_hessian:

  logical. If TRUE, compute an approximate Hessian matrix based on the
  optimization procedure. Default is FALSE.

- nfcasts:

  number of forecasts. Default is 0.

- log:

  logical. If TRUE, the model is estimated on the log-scale. Default is
  FALSE.

- series_time:

  optional vector of time indices associated with `series`.

## Value

An object of class `"hf_estimation"` containing:

- the original and linearized series,

- estimated regression effects and outlier components,

- model parameters and covariance matrices,

- likelihood and diagnostic information.

## Details

Automatic outlier detection can be enabled by specifying the outlier
types and a critical value for the detection threshold.

## Examples

``` r
# Simulated examples with and without regressor
set.seed(125)
reg <- data.frame(
   reg1 = round(runif(1000, min = 0, max = 1))*2-1,
 reg2 = round(runif(1000, min = 0, max = 1))*2-1
)

y = 100 + 2*reg$reg1 -2*reg$reg2 + rnorm(nrow(reg))

# input data
data <- list(
   series = y,
   date = seq.Date(from = as.Date("2020-01-01"),
   by = "day",
                   length.out = 1000)
)

# Linearize the series using weekly and annual periodicities
est <- fractional_airline_estimation(
   data$series,
   period = c(7, 30.4),
   log = FALSE,
   xreg = reg,
   series_time = data$date
)
#> Error in .jcall(obj = "jdplus/toolkit/base/api/math/matrices/Matrix",     returnSig = "Ljdplus/toolkit/base/api/math/matrices/Matrix;",     method = "of", .jarray(as.double(s)), as.integer(sdim[1]),     as.integer(sdim[2])): RcallMethod: cannot determine object class

est
#> Error: object 'est' not found

est2 <- fractional_airline_estimation(
  data$series,
  period = c(7, 30.4),
  log = FALSE,
  series_time = data$date
)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/math/matrices/Matrix has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0

est2
#> Error: object 'est2' not found
```
