# Perform an Arima Model Based (AMB) decomposition

Performs an Arima Model Based (AMB) decomposition using a (fractional)
airline model, suitable for high-frequency time series. The method
decomposes the input series into trend, seasonal and irregular
components, with optional signal–noise decomposition.

## Usage

``` r
fractional_airline_decomposition(
  series,
  period,
  sn = FALSE,
  stde = FALSE,
  nbcasts = 0,
  nfcasts = 0,
  eps = 1e-09,
  deps = 1e-04,
  log = FALSE,
  series_time = NULL
)
```

## Arguments

- series:

  input time series.

- period:

  period of the seasonal component, any positive real number.

- sn:

  decomposition into signal and noise (2 components only). The signal is
  the seasonally adjusted series and the noise the seasonal component.
  Default: FALSE.

- stde:

  Boolean: TRUE: compute standard deviations of the components. In some
  cases (e.g. memory limits), it is currently not possible to compute
  them. Default: FALSE.

- nbcasts:

  number of backcasts. Default: 0.

- nfcasts:

  number of forecasts. Default: 0.

- eps:

  precision of the optimisation routine. Default:1e-9.

- deps:

  step in the computation of the numerical derivatives, used in the
  optimisation routine. Default:1e-4

- log:

  logical indicating whether the series is on the log scale. Default:
  FALSE.

- series_time:

  vector of times at which \`series\` is indexed. Optional.

## Value

An object containing the AMB decomposition results, including the
estimated components and, if requested, their standard deviations.

## Details

If \`sn = TRUE\`, the decomposition is restricted to two components only
(signal and noise). When \`stde = TRUE\`, the computation of standard
deviations may fail for long series or high-frequency data due to memory
constraints.

## Examples

``` r
series <- rnorm(70)+100

### Example with a daily time series with a day-of-the-week effect
weekday <- rjd3highfreq::fractional_airline_decomposition(
  series,
  period = 7,
  log = TRUE,
  series_time = seq.Date(from=as.Date("2025-01-01"),
                         by = "days",
                         length.out = length(series))
)
#> Error in rJava::.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",     "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",     "decompose", as.numeric(series), as.numeric(period), sn,     stde, as.integer(nbcasts), as.integer(nfcasts), as.numeric(eps),     as.numeric(deps)): RcallMethod: cannot determine object class

### Example with a weekly time series
series <-  rnorm(200)+100
weekly <- rjd3highfreq::fractional_airline_decomposition(
  series,
  period = 52.18,
  log =  TRUE,
  series_time = seq.Date(from=as.Date("2025-01-01"),
                         by = "days",
                         length.out = length(series))
)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/highfreq/base/r/FractionalAirlineProcessor has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
