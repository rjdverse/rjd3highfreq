#' Compute IC-Ratio
#'
#' @param series input time series
#' @param frequency frequency of the seasonal component. Can be NULL if series is ts
#' @param trend_length length of Henderson filter used for trend adjustment. If NULL it is nextodd(frequency)
#' @param musgrave Boolean indicating if Musgrave asymmetric filters should be used in trend adjustment
#' @param multiplicative boolean indicating if the decomposition is multiplicative or additive
#' @param seasonally_adjust boolean. Should series be seasonally adjusted?
#' @param sma seasonal moving average used in x11plus()
#' @param ... additional arguments for trend adjustment. See ?henderson
#' @details In the traditional calculation of the IC-ratio, no asymmetric filters used for the preliminary
#' trend estimation (see Ladiray and Quenneville 2001, Table B7).
#' @references Ladiray, D., Quenneville, B. (2001). The Various Tables. In: Seasonal Adjustment with the X-11 Method. Lecture Notes in Statistics, vol 158. Springer, New York, NY. https://doi.org/10.1007/978-1-4613-0175-2_5
#' @examples
#' series <- rjd3toolkit::Retail$AllOtherGenMerchandiseStores
#' ic_ratio(series)
#'
#' @export

ic_ratio <- function(series,
                     frequency = NULL,
                     trend_length = NULL,
                     musgrave = FALSE,
                     multiplicative = FALSE,
                     seasonally_adjust = TRUE,
                     sma = "S3X5",
                     ...) {

  if (is.null(frequency)) {
    if (inherits(series, "ts")) {
      frequency <- stats::frequency(series)
    } else {
      stop("frequency cannot be NULL if series is not a ts", call. = FALSE)
    }
  }

  if (is.null(trend_length)) { # trend_length = next_odd(frequency) by default
    x_floor <- floor(frequency)

    if (x_floor %% 2L == 0L) {
      trend_length <- x_floor + 1
    } else {
      trend_length <- x_floor + 2
    }
  }

  ## Get seasonally adjusted series using X-11
  if (seasonally_adjust) {
    result <- x11plus(
      series,
      period = frequency,
      multiplicative = multiplicative,
      seas_s0 = sma,
      seas_s1 = sma
    )

    series_sa <- result$decomposition$sa
  } else {
    series_sa <- series
  }

  ## Get trend adjusted
  trend <- henderson(series_sa,
                     length = trend_length,
                     musgrave = musgrave,
                     ...)

  ## Shorten seasonally adjusted and trend series if need
  remove_na <- is.na(series_sa) | is.na(trend)
  series_sa <- as.numeric(series_sa)[!remove_na]
  trend <- as.numeric(trend)[!remove_na]

  ## Calculate ic-ratio
  result <- rJava::.jcall("jdplus/x12plus/base/r/X11Decomposition",
                          "D",
                          "icratio",
                          series_sa,
                          trend,
                          multiplicative)

  return(result)
}
