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

## Value

A Java RegArima model

## Examples

``` r
jspec<-.extended_airline_spec(c(12))
.extended_airline_regarima(rjd3toolkit::ABS$X0.2.09.10.M, jspec)
#> [1] "Java-Object{jdplus.toolkit.base.core.regarima.RegArimaModel@2b2948e2}"
```
