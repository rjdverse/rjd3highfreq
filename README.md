
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3highfreq

High-frequency time series

## Installation

``` r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit")
remotes::install_github("rjdemetra/rjd3sts")
remotes::install_github("rjdemetra/rjd3highfreq")
```

## Demonstration with the daily french births

``` r
library("rjd3highfreq")
```

``` r
## Import des données ----------------------------------------------------------

df_daily <- read.csv2("https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv")

# Creation of log variables to multiplicative model
df_daily$log_births = log(df_daily$births)
df_daily$date = as.Date(df_daily$date)
```

Plot of the raw series:

<img src="man/figures/README-raw data plot-1.png" width="100%" style="display: block; margin: auto;" />

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
  "1968-01-01", length = nrow(df_daily), type = "All", nonworking = 7L)
colnames(cal_reg) <- c("14th_july", "8th_may", "1st_jan", "1st_may", 
                       "east_mon", "asc", "pen_mon", 
                       "15th_aug", "1st_nov", "11th_nov", "Xmas")
```

Preprocessing with the function `fractionalAirlineEstimation`:

``` r
pre.mult <- fractionalAirlineEstimation(
  y = df_daily$log_births, 
  x = cal_reg, 
  periods = 7, # weekly frequency
  outliers = c("ao", "wo"))

print(pre.mult)
#> 
#> Estimate MA parameters:
#>       MA_parameter      Coef     Coef_SE    Tstat
#>           Theta(1) 0.7620698 0.005571472 136.7807
#>  Theta(period = 7) 0.9731793 0.001413477 688.5002
#> 
#> Number of calendar regressors: 11 , Number of outliers : 7
#> 
#> TD regressors coefficients:
#>   Variable    Coef Coef_SE    Tstat
#>  14th_july -0.1226  0.0047 -26.0615
#>    8th_may -0.1419  0.0054 -26.3419
#>    1st_jan -0.2223  0.0047 -47.3511
#>    1st_may -0.1225  0.0047 -26.2643
#>   east_mon -0.1891  0.0046 -40.7635
#>        asc -0.1726  0.0046 -37.1949
#>    pen_mon -0.1900  0.0046 -40.9429
#>   15th_aug -0.1181  0.0047 -25.3461
#>    1st_nov -0.1503  0.0046 -32.5662
#>   11th_nov -0.1238  0.0046 -26.8142
#>       Xmas -0.2310  0.0046 -49.7435
#> 
#> Outliers coefficients:
#>  Variable    Coef Coef_SE   Tstat
#>  WO.11688 -0.1762  0.0226 -7.7916
#>  AO.10089 -0.2224  0.0340 -6.5503
#>  WO.11681 -0.1447  0.0226 -6.3981
#>  AO.16072  0.2098  0.0340  6.1786
#>  AO.11153 -0.2101  0.0340 -6.1880
#>  AO.10788 -0.2092  0.0340 -6.1602
#>   AO.9983 -0.2042  0.0340 -6.0146
#> 
#> Number of observations: 19359
#> Sum of square residuals: 25.17 on 19330 degrees of freedom
#> Log likelihood = 3.682e+04, 
#>  aic = -7.361e+04, 
#>  aicc = -7.361e+04, 
#>  bic(corrected for length) = -6.635
#> Hannan–Quinn information criterion = -7.355e+04
```

Comparison between raw data and linearised series:

<img src="man/figures/README-plot linearised series-1.png" width="100%" style="display: block; margin: auto;" />

Decomposition with the AMB (Arima Model Based) algorithm:

``` r
# Decomposition with day of the week
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  y = pre.mult$model$linearized, 
  period = 7) # weekly decomposition

# Extract DOY pattern from DOW-adjusted linearised data
# step 2 en log
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  y = amb.dow$decomposition$sa, # DOW-adjusted linearised data
  period = 365.2425) # day of year pattern
```

Comparison of the two seasonnal components ($p = 365$ and $p = 7$):

<img src="man/figures/README-plot seasonnal amb-1.png" width="100%" style="display: block; margin: auto;" />

<img src="man/figures/README-plot zoom seasonnal amb-1.png" width="100%" style="display: block; margin: auto;" />

Result of the decomposition (Raw data, Trend and seasonaly adjusted
series):

<img src="man/figures/README-plot decompo amb-1.png" width="100%" style="display: block; margin: auto;" />

Perform an Arima Model Based (AMB) decomposition on several periodcities
at once:

Plot the comparison between the two AMB methods:

<img src="man/figures/README-plot seasonnal amb.multi-1.png" width="100%" style="display: block; margin: auto;" />

Plot the output of the `multiAirlineDecomposition`:

<img src="man/figures/README-plot decompo amb.multi-1.png" width="100%" style="display: block; margin: auto;" />

With the package
[**rjd3x11plus**](https://github.com/rjdemetra/rjd3x11plus), you can
perform an X-11 like decomposition with any (non integer) periodicity.
