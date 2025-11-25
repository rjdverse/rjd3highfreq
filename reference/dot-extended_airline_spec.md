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
#> [1] "Java-Object{ExtendedAirlineSpec(mean=false, periodicities=[7.0, 365.25], differencingOrder=3, phi=null, theta=..., stheta=[..., ...], adjustToInt=false)}"
```
