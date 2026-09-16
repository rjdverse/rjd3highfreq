# 'X11' Extreme Values Corrector

'X11' Extreme Values Corrector

## Usage

``` r
extreme_values_correction(
  series,
  period,
  corrected_s,
  lsigma = 1.5,
  usigma = 2.5,
  multiplicative = FALSE,
  start = 0,
  clean_extremities = TRUE
)
```

## Arguments

- series:

  the analysed time series.

- period:

  the period of the input time series if \`series\` is not a \`"ts"\`
  object.

- corrected_s:

  other time series if the series being corrected is different from
  series.

- lsigma:

  the lower sigma boundaries for the detection of extreme values.

- usigma:

  the upper sigma boundaries for the detection of extreme values.

- multiplicative:

  boolean indicating if the decomposition is multiplicative or additive.

- start:

  position of the first "complete" considered period.

- clean_extremities:

  boolean indicating if the extremities should be cleaned.

## Details

The 'X11' Extreme Values Corrector is used to compute the tables b4,
b4g, b9, b9g, b17, b20, c17 and c20.

\#' The returned correction contains the following columns:

- `obs_weight`:

  Extreme value corrections weight of each observation

- `correction_factors`:

  Extreme value correction factors

## Examples

``` r
extreme_values_correction(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#> Error in rJava::.jcall("jdplus/sa/base/api/DecompositionMode", "Ljdplus/sa/base/api/DecompositionMode;",     "valueOf", ifelse(multiplicative, "Multiplicative", "Additive")): RcallMethod: cannot determine object class
```
