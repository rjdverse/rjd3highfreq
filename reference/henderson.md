# Apply Henderson linear filter

Apply Henderson linear filter

## Usage

``` r
henderson(series, length, musgrave = TRUE, ic = 4.5)
```

## Arguments

- series:

  Input time series.

- length:

  Length of the symmetric Henderson filter. Must be odd.

- musgrave:

  Boolean indicating if Musgrave asymmetric filters should be used.
  Default is `TRUE`. If `FALSE`, no asymmetric filters will be used.

- ic:

  I/C ratio between the volatility of the tentative irregular and
  trend-cycle estimates (needed for calculating the Musgrave asymmetric
  filters).

## Value

A numeric array corresponding to the estimated trend.

## Examples

``` r
q <- x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#> Error in rJava::.jcall("jdplus/x12plus/base/r/X11Decomposition", "Ljdplus/x12plus/base/r/X11Decomposition$Results;",     "process", as.numeric(series), as.numeric(period), multiplicative,     as.integer(trend_horizon), as.integer(trend_degree), tkernel,     asym, seas0, seas1, extreme_lsig, extreme_usig): RcallMethod: cannot determine object class

henderson(q$decomposition$sa, 13)
#> Error in .jcheck(): java.lang.UnsupportedClassVersionError: jdplus/x12plus/base/r/X11Decomposition has been compiled by a more recent version of the Java Runtime (class file version 65.0), this version of the Java Runtime only recognizes class file versions up to 61.0
```
