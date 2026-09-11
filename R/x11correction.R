#' 'X11' Extreme Values Corrector
#'
#' @param series the analysed time series.
#' @param period the period of the input time series if `series` is not a `"ts"` object.
#' @param corrected_s other time series if the series being corrected is different from series.
#' @param lsigma the lower sigma boundaries for the detection of extreme values.
#' @param usigma the upper sigma boundaries for the detection of extreme values.
#' @param multiplicative boolean indicating if the decomposition is multiplicative or additive.
#' @param start position of the first "complete" considered period.
#' @param clean_extremities boolean indicating if the extremities should be cleaned.
#'
#' @details
#' The 'X11' Extreme Values Corrector is used to compute the tables
#' b4, b4g, b9, b9g, b17, b20, c17 and c20.
#'
#' #' The returned correction contains the following columns:
#'
#' \describe{
#'   \item{\code{obs_weight}}{Extreme value corrections weight of each observation}
#'   \item{\code{correction_factors}}{Extreme value correction factors}
#' }
#'
#' @examples
#' extreme_values_correction(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#'
#' @export

extreme_values_correction <- function(series,
                                      period,
                                      corrected_s,
                                      lsigma = 1.5,
                                      usigma = 2.5,
                                      multiplicative = FALSE,
                                      start = 0,
                                      clean_extremities = TRUE) {
  if (missing(period)) {
    if (stats::is.ts(series)) {
      period <- stats::frequency(series)
    } else {
      stop("The period parameter must be defined")
    }
  }

  p <- rJava::.jcast(rJava::new(rJava::J("java.lang.Double"), as.character(period)), "java.lang.Number")
  dmode <- rJava::.jcall(
    "jdplus/sa/base/api/DecompositionMode",
    "Ljdplus/sa/base/api/DecompositionMode;",
    "valueOf",
    #ifelse(multiplicative, "mul", "Additive")
    ifelse(multiplicative, "Multiplicative", "Additive")
  )
  if (clean_extremities) {
    series_c <- rjd3toolkit::clean_extremities(series)
  } else {
    series_c <- series
  }

  jseries <- .r2jd_doubleseq(series_c)
  x11context <- rJava::J("jdplus/x12plus/base/core/X11Context")$builder()$mode(dmode)$period(p)$lowerSigma(lsigma)$upperSigma(usigma)$build()
  dfvc <- rJava::.jnew("jdplus/x12plus/base/core/DefaultExtremeValuesCorrector")
  rJava::.jcall(dfvc, "V", "setStart", as.integer(start))
  rJava::.jcall(dfvc, "V", "analyse", jseries, x11context)
  rJava::.jcall(dfvc, "V", "analyse", jseries, x11context)
  obs_w <- rJava::.jcall(
    rJava::.jcall(
      dfvc,
      "Ljdplus/toolkit/base/api/data/DoubleSeq;",
      "getObservationWeights"
    ),
    "[D",
    "toArray"
  )
  if (missing(corrected_s)) {
    corr_f <-  rJava::.jcall(
      rJava::.jcall(
        dfvc,
        "Ljdplus/toolkit/base/api/data/DoubleSeq;",
        "getCorrectionFactors"
      ),
      "[D",
      "toArray"
    )
  } else {
    if (clean_extremities) {
      corrected_s <- rjd3toolkit::clean_extremities(corrected_s)
    }
    corr_f <- rJava::.jcall(
      rJava::.jcall(
        dfvc,
        "Ljdplus/toolkit/base/api/data/DoubleSeq;",
        "computeCorrections",
        .r2jd_doubleseq(corrected_s)
      ),
      "[D",
      "toArray"
    )
  }
  res <- cbind(obs_w, corr_f)
  colnames(res) <- c("obs_weight", "correction_factors")
  if (stats::is.ts(series)) {
    res <- stats::ts(res,
              start = stats::start(series_c),
              frequency = stats::frequency(series_c))
    res <- stats::window(res,
                         start = stats::start(series),
                         end = stats::end(series),
                         extend = TRUE)
  }

  return(res)
}
