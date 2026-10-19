
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {rjd3highfreq} <a href="https://rjdverse.github.io/rjd3highfreq/"><img src="man/figures/logo.png" align="right" height="150" style="float:right; height:150px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rjd3highfreq)](https://CRAN.R-project.org/package=rjd3highfreq)
[![Mentioned in Awesome Official
Statistics](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software)

[![R-CMD-check](https://github.com/rjdverse/rjd3highfreq/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/rjdverse/rjd3highfreq/actions/workflows/R-CMD-check.yml)
[![lint](https://github.com/rjdverse/rjd3highfreq/actions/workflows/lint.yml/badge.svg)](https://github.com/rjdverse/rjd3highfreq/actions/workflows/lint.yml)

[![GH Pages
built](https://github.com/rjdverse/rjd3highfreq/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/rjdverse/rjd3highfreq/actions/workflows/pkgdown.yml)
<!-- badges: end -->

{rjd3highfreq} provides functions for seasonal adjustment of
high-frequency data displaying multiple, non integer periodicities.
Pre-adjustment with extended airline model and Arima Model Based
decomposition.

## Installation

Running rjd3 packages requires **Java 21 or higher**. How to set up such
a configuration in R is explained
[here](https://jdemetra-new-documentation.netlify.app/#Rconfig)

### Latest release

To get the current stable version (from the latest release):

- From GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3highfreq@*release")
```

- From [r-universe](https://rjdverse.r-universe.dev/rjd3highfreq):

``` r
install.packages("rjd3highfreq", repos = c("https://rjdverse.r-universe.dev", "https://cloud.r-project.org"))
```

### Development version

You can install the development version of **{rjd3highfreq}** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3highfreq")
```

## Demonstration with the daily french births

``` r
library("rjd3highfreq")
#> 
#> Attaching package: 'rjd3highfreq'
#> The following object is masked from 'package:stats':
#> 
#>     loess
```

``` r
## Import of data
df_daily <- read.csv2("https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv")

# Creation of log variables to multiplicative model
df_daily$log_births <- log(df_daily$births)
df_daily$date <- as.Date(df_daily$date)
```

Plot of the raw series:

Preparation of the calendar with the package **rjd3toolkit**:

``` r
# French calendar
frenchCalendar <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(7, 14), # Bastille Day
  rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
  rjd3toolkit::special_day('NEWYEAR'),
  rjd3toolkit::special_day('MAYDAY'), # 1st may
  rjd3toolkit::special_day('EASTERMONDAY'),
  rjd3toolkit::special_day('ASCENSION'),
  rjd3toolkit::special_day('WHITMONDAY'),
  rjd3toolkit::special_day('ASSUMPTION'),
  rjd3toolkit::special_day('ALLSAINTSDAY'), # Toussaint
  rjd3toolkit::special_day('ARMISTICE'), # End of 1st WW
  rjd3toolkit::special_day('CHRISTMAS'))
)
```

Creation of the calendar regressor in a matrix with the package
**rjd3toolkit**:

``` r
# Calendar regressor matrix
cal_reg <- rjd3toolkit::holidays(
    calendar = frenchCalendar,
    start = "1968-01-01", length = nrow(df_daily),
    type = "All", nonworking = 7L)

colnames(cal_reg) <- c("14th_july", "8th_may", "1st_jan", "1st_may",
                       "east_mon", "asc", "pen_mon",
                       "15th_aug", "1st_nov", "11th_nov", "Xmas")
```

Preprocessing with the function `fractionalAirlineEstimation`:

``` r
pre_pro <- fractional_airline_estimation(
    series = df_daily$births,
    xreg = cal_reg,
    period = 7, # weekly frequency
    outliers = c("ao", "wo"), log = TRUE, series_time = df_daily$date)

print(pre_pro)
#> 
#> 
#> Fractional Airline estimation:
#> 
#>   period           : 7.000
#>   series_time      : 1968-01-01,  ..., 2020-12-31
#>   parameters       : 0.762, 0.973
#>   t                : 136.884, 774.367
#>   score            : 2.088, 24.448
#>   covariance       : [0.000, 0.000], [0.000, 0.000]
#>   log              : TRUE
#> 
#> 
#> Regression results:
#> 
#>                    coef        t
#>   -------------  ------  -------
#>   14th_july      -0.123  -26.062
#>   8th_may        -0.142  -26.342
#>   1st_jan        -0.222  -47.351
#>   1st_may        -0.122  -26.264
#>   east_mon       -0.189  -40.764
#>   asc            -0.173  -37.195
#>   pen_mon        -0.190  -40.943
#>   15th_aug       -0.118  -25.346
#>   1st_nov        -0.150  -32.566
#>   11th_nov       -0.124  -26.814
#>   Xmas           -0.231  -49.744
#>   WO.1999-12-31  -0.176   -7.792
#>   AO.1995-08-15  -0.222   -6.550
#>   WO.1999-12-24  -0.145   -6.398
#>   AO.2012-01-01   0.210    6.179
#>   AO.1998-07-14  -0.210   -6.188
#>   AO.1997-07-14  -0.209   -6.160
#>   AO.1995-05-01  -0.204   -6.015
#> 
#> 
#> Likelihood:
#> 
#>   ll               : 36823.915
#>   ssq              : 25.169
#>   nobs             : 19359.000
#>   neffectiveobs    : 19351.000
#>   nparams          : 21.000
#>   df               : 19330.000
#>   aic              : -73605.830
#>   aicc             : -73605.782
#>   bic              : -73440.549
#>   bic2             : -3.795
#>   bicc             : -6.635
#>   hannanquinn      : -73551.669
```

``` r
plot(pre_pro, main = "French births")
```

<img src="man/figures/README-preprocessing plots-1.png" alt="" width="100%" style="display: block; margin: auto;" />

``` r
plot(x = pre_pro,
     from = as.Date("2000-01-01"), to = as.Date("2000-12-31"),
     main = "French births in 2000")
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "from" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "to" is not a graphical
#> parameter
```

<img src="man/figures/README-preprocessing plots-2.png" alt="" width="100%" style="display: block; margin: auto;" />

Decomposition with the AMB (Arima Model Based) algorithm:

``` r
# Decomposition with weekly pattern
amb.dow <- rjd3highfreq::fractional_airline_decomposition(
    series = pre_pro$decomposition$linearized, # linearized series from preprocessing
    period = 7,
    log = TRUE, series_time = df_daily$date)

# Extract day-of-year pattern from day-of-week-adjusted linearised data
amb.doy <- rjd3highfreq::fractional_airline_decomposition(
    series = amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # day of year pattern
    log = TRUE, series_time = df_daily$date)
```

Plot:

``` r
plot(amb.dow)
```

<img src="man/figures/README-amb plot-1.png" alt="" width="100%" style="display: block; margin: auto;" />

``` r
plot(amb.doy)
```

<img src="man/figures/README-amb plot-2.png" alt="" width="100%" style="display: block; margin: auto;" />

Perform an Arima Model Based (AMB) decomposition on several periodcities
at once:

``` r
amb.multi <- rjd3highfreq::multi_airline_decomposition(
  series = pre_pro$decomposition$linearized, # input time series
  period = c(7, 365.2425), # 2 frequency
  log = TRUE, series_time = df_daily$date)
```

Plot the comparison between the two AMB methods for the annual
periodicity:

``` r
plot(amb.multi)
```

<img src="man/figures/README-plot amb.multi 1-1.png" alt="" width="100%" style="display: block; margin: auto;" />

With the package
[**rjd3x11plus**](https://github.com/rjdverse/rjd3x11plus), you can
perform an X-11 like decomposition with any (non integer) periodicity.

## Package Maintenance and contributing

Any contribution is welcome and should be done through pull requests
and/or issues. pull requests should include **updated tests** and
**updated documentation**. If functionality is changed, docstrings
should be added or updated.

## Licensing

The code of this project is licensed under the [European Union Public
Licence
(EUPL)](https://interoperable-europe.ec.europa.eu/collection/eupl/eupl-text-eupl-12).
