# Creates a java RegArima models based on an extended airline spec

Creates a java RegArima models based on an extended airline spec

## Usage

``` r
.extended_airline_regarima(y, jspec, mean = FALSE, X = NULL)
```

## Arguments

- y:

  y

- jspec:

  Java spec

- mean:

  Mean correction (to be avoided)

- X:

  Regression variables

- deps:

  step identifying small changes in the parameters (used in the
  computation of numerical derivatives). Will be removed.

## Value

A Java RegArima model

## Examples

``` r
jspec<-.extended_airline_spec(c(12))
#> Error in .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/api/ExtendedAirlineSpec;",     "spec", .jarray(as.numeric(periodicities)), as.integer(differencing),     as.logical(ar), as.logical(toint)): java.lang.UnsupportedClassVersionError: jdplus/toolkit/base/api/information/InformationExtractors has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
.extended_airline_regarima(rjd3toolkit::ABS$X0.2.09.10.M, jspec)
#> Error: object 'jspec' not found
```
