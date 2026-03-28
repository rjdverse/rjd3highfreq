# Internal routine to create an ExtendedAirlineSpec

Internal routine to create an ExtendedAirlineSpec

## Usage

``` r
.extended_airline_spec(
  periodicities,
  differencing = -1,
  ar = FALSE,
  toint = FALSE
)
```

## Arguments

- periodicities:

  Periodicities

- differencing:

  Differnecing order. -1 for automatic computation

- ar:

  Use of an AR regular stationary polynomial instead of a MA polynomial

- toint:

  Round periodicties to integers

## Value

A Java ExtendedAirlineSpec object

## Examples

``` r
.extended_airline_spec(c(7, 365.25))
#> Error in .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/api/ExtendedAirlineSpec;",     "spec", .jarray(as.numeric(periodicities)), as.integer(differencing),     as.logical(ar), as.logical(toint)): RcallMethod: cannot determine object class
```
