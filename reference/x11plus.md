# Modified X-11 seasonal adjustment

This function implements a modified version of the classical X-11
seasonal adjustment approach. The modified X-11 method is applicable to
time series with arbitrary seasonal periodicity.

## Usage

``` r
x11plus(
  series,
  period,
  multiplicative = TRUE,
  trend_horizon = 6,
  trend_degree = 3,
  trend_kernel = c("Henderson", "BiWeight", "TriWeight", "TriCube", "Uniform",
    "Triangular", "Epanechnikov", "Trapezoidal"),
  trend_asymmetric = c("CutAndNormalize", "Direct", "MMSRE"),
  trend_coefs,
  seas_s0 = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
  seas_s1 = c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
  extreme_lsig = 1.5,
  extreme_usig = 2.5,
  user_defined = NULL
)
```

## Arguments

- series:

  Input time series.

- period:

  Seasonal periodicity of `series`. Must be a positive real number
  larger than or equal to 2.

- multiplicative:

  Decomposition mode for `series` (Boolean). If `TRUE` (default), then
  `series` is decomposed into multiplicative trend-cyclical, seasonal,
  and irregular components; if `FALSE`, then it is decomposed into
  additive unobserved components.

- trend_horizon:

  Bandwidth of the symmetric local polynomial regression filter. Default
  is 6, giving a 13-term filter. See details.

- trend_degree:

  Polynomial degree that should be preserved by the symmetric local
  polynomial regression filter. Default is 3, giving preservation of
  cubic polynomials. See details.

- trend_kernel:

  A kernel defining the weights in the objective function. See details.

- trend_asymmetric:

  Approach for deriving asymmetric local polynomial regression filters.
  See details.

- trend_coefs:

  A filter bank containing the weights of the symmetric trend-cycle
  filter and its asymmetric variants. Can be an object of class
  `"list"`, `"matrix"`, `"lp_filter"` or `"rkhs_filter"`. See details.

- seas_s0:

  \\3 \times k\\ seasonal filter for preliminary seasonal estimation
  (B5, C5, D5). Default is \\3 \times 3\\.

- seas_s1:

  \\3 \times k\\ seasonal filter for refined and final seasonal
  estimation (B10, C10, D10). Default is \\3 \times 5\\.

- extreme_lsig:

  Lower \\\sigma\\-limit for extreme-value correction in the
  seasonal-irregular component. Must be non-negative, default is 1.5.

- extreme_usig:

  Upper \\\sigma\\-limit for extreme-value correction in the
  seasonal-irregular component. Must be greater than `extreme_lsig`,
  default is 2.5.

- user_defined:

  A vector containing additional output tables. Default is `NULL`.

## Value

An object of class `"hf_decomposition"`. It contains the specified
parameters and a matrix that stores the input `series` and the final
estimates of the seasonally adjusted series (D11), the trend-cyclical
component (D12), the seasonal component (D10), and the irregular
component (D13).

## Details

The main novelty of the modified X-11 method is the implementation of
advanced options for refined and final trend-cycle estimation (B7, C7,
D7, and D12), generalising the use of classical Henderson filters. The
following logic applies, see also Webel (2026) and Webel and Smyk (2024)
for technical details:

- If `trend_coefs` is unspecified (default), then the local polynomial
  regression filters suggested in Proietti and Luati (2008) are applied.
  The underlying regression model, from which the symmetric trend-cycle
  filter arises, is further specified through the `trend_horizon`,
  `trend_degree`, and `trend_kernel` parameters:

  - 2 \* `trend_horizon` + 1 is the number of observations to be
    considered in the local trend approximation and, hence, the length
    of the resulting symmetric trend-cycle filter.

  - `trend_degree` is the order of the polynomial in the local trend
    approximation.

  - `trend_kernel` is a kernel function that defines the sequence of
    non-negative weights in the objective function, which is then
    minimised with respect to the regression parameters.

  In addition, `trend_asymmetric` defines the method to be used for
  deriving the requisite asymmetric local polynomial regression filters.
  Three methods are currently available: the cut-and-normalise approach
  of Gasser and Müller (1979) (`CutAndNormalize`), the direct asymmetric
  filters suggested in Proietti and Luati (2008) (`Direct`), and the
  minimum mean squared revision error approach developed in Grun-Rehomme
  et al. (2018) (`MMSRE`).

- If `trend_coefs` is specified, then all other parameters related to
  trend-cycle estimation are ignored, and the provided trend-cycle
  filter bank is applied. Note that the rjd3filters package can be used
  to create filters, e.g. by setting trend_coefs =
  rjd3filters::rkhs_filter().

Two final warnings regarding seasonal estimation.

1.  Extremes in the seasonal-irregular component are currently corrected
    only in iterations B and C, where the moving irregular standard
    deviation is always calculated from 5-year moving windows. That is,
    no final replacement values (D9) are currently computed.

2.  Users must specify seasonal filters `seas_s0` and `seas_s1` that
    match the length of `series`, as there is currently no automatic
    resetting of ill-specified seasonal filters. That is, any
    specification of seasonal filters that are too long for the given
    `series` will simply produce an error message.

## References

Dagum, E. B. and S. Bianconcini (2008). The Henderson Smoother in
Reproducing Kernel Hilbert Space. Journal of Business and Economic
Statistics 26 (4), 536–545. <https://doi.org/10.1198/073500107000000322>

Gasser, T. and H.-G. Müller (1979). Kernel Estimation of Regression
Functions. In T. Gasser and M. Rosenblatt (Eds), Smoothing Techniques
for Curve Estimation, 23–68. Heidelberg: Springer.
<https://doi.org/10.1007/BFb0098489>

Grun-Rehomme, M., F. Guggemos and D. Ladiray (2018). Asymmetric Moving
Averages Minimizing Phase Shift. In G. L. Mazzi, D. Ladiray and D. A.
Riester (Eds), Handbook on Seasonal Adjustment, 391–413. Luxembourg:
Publications Office of the European Union.

Proietti, T. and A. Luati (2008). Real Time Estimation in Local
Polynomial Regression, with Application to Trend-Cycle Analysis. Annals
of Applied Statistics 2 (4), 1523–1553.
<https://doi.org/10.1214/08-AOAS195>

Webel, K. (2026). Some Thoughts on Modified X-11 Seasonal Adjustments
for Time Series with Complex Seasonality. Deutsche Bundesbank Discussion
Paper XX/2026. Forthcoming

Webel, K. and A. Smyk (2024). Seasonal Adjustment of Infra-Monthly Time
Series with JDemetra+. Journal of Official Statistics 40 (4), 783–828.
<https://doi.org/10.1177/0282423X241277602>

## Examples

``` r
x11decomp  <- x11plus(
series = rjd3toolkit::ABS$X0.2.09.10.M,
period = 12,
trend_horizon = 99
)
#> Error in rJava::.jcall("jdplus/x12plus/base/r/X11Decomposition", "Ljdplus/x12plus/base/r/X11Decomposition$Results;",     "process", as.numeric(series), as.numeric(period), multiplicative,     as.integer(trend_horizon), as.integer(trend_degree), tkernel,     asym, seas0, seas1, extreme_lsig, extreme_usig): RcallMethod: cannot determine object class
```
