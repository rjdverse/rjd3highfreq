#' Modified X-11 seasonal adjustment
#'
#' This function implements a modified version of the classical X-11 seasonal adjustment approach. The modified X-11 method is applicable to time series with arbitrary seasonal periodicity.
#' @param series Input time series.
#' @param period Seasonal periodicity of \code{series}. Must be a positive real number larger than or equal to 2.
#' @param multiplicative Decomposition mode for \code{series} (Boolean). If \code{TRUE} (default), then \code{series} is decomposed into multiplicative
#' trend-cyclical, seasonal, and irregular components; if \code{FALSE}, then it is decomposed into additive unobserved components.
#' @param trend_horizon Bandwidth of the symmetric local polynomial regression filter. Default is 6, giving a 13-term filter. See details.
#' @param trend_degree Polynomial degree that should be preserved by the symmetric local polynomial regression filter. Default is 3, giving preservation of cubic polynomials. See details.
#' @param trend_kernel A kernel defining the weights in the objective function. See details.
#' @param trend_asymmetric Approach for deriving asymmetric local polynomial regression filters. See details.
#' @param trend_coefs A filter bank containing the weights of the symmetric trend-cycle filter and its asymmetric variants. Can be an object
#' of class \code{"list"}, \code{"matrix"}, \code{"lp_filter"} or \code{"rkhs_filter"}. See details.
#' @param seas_s0 \eqn{3 \times k} seasonal filter for preliminary seasonal estimation (B5, C5, D5). Default is \eqn{3 \times 3}.
#' @param seas_s1 \eqn{3 \times k} seasonal filter for refined and final seasonal estimation (B10, C10, D10). Default is \eqn{3 \times 5}.
#' @param extreme_lsig Lower \eqn{\sigma}-limit for extreme-value correction in the seasonal-irregular component. Must be non-negative, default is 1.5.
#' @param extreme_usig Upper \eqn{\sigma}-limit for extreme-value correction in the seasonal-irregular component. Must be greater than \code{extreme_lsig}, default is 2.5.
#' @param user_defined A vector containing additional output tables. Default is \code{NULL}.
#'
#' @details
#' The main novelty of the modified X-11 method is the implementation of advanced options for refined and final trend-cycle estimation (B7, C7, D7, and D12), generalising the use of classical Henderson filters.
#' The following logic applies, see also Webel (2026) and Webel and Smyk (2024) for technical details:
#' \itemize{
#' \item If \code{trend_coefs} is unspecified (default), then the local polynomial regression filters suggested in Proietti and Luati (2008) are applied. The underlying regression model, from which the symmetric
#' trend-cycle filter arises, is further specified through the \code{trend_horizon}, \code{trend_degree}, and \code{trend_kernel} parameters:
#'  \itemize{
#'  \item 2 * \code{trend_horizon} + 1 is the number of observations to be considered in the local trend approximation and, hence, the length of the resulting symmetric trend-cycle filter.
#'  \item \code{trend_degree} is the order of the polynomial in the local trend approximation.
#'  \item \code{trend_kernel} is a kernel function that defines the sequence of non-negative weights in the objective function, which is then minimised with respect to the regression parameters.
#'  }
#' In addition, \code{trend_asymmetric} defines the method to be used for deriving the requisite asymmetric local polynomial regression filters. Three methods are currently available: the cut-and-normalise approach
#' of Gasser and Müller (1979) (\code{CutAndNormalize}), the direct asymmetric filters suggested in Proietti and Luati (2008) (\code{Direct}), and the minimum mean squared revision error approach developed in
#' Grun-Rehomme et al. (2018) (\code{MMSRE}).
#' \item If \code{trend_coefs} is specified, then all other parameters related to trend-cycle estimation are ignored, and the provided trend-cycle filter bank is applied. Note that the \pkg{rjd3filters} package can
#' be used to create filters, e.g. by setting trend_coefs = rjd3filters::rkhs_filter().
#' }
#' Two final warnings regarding seasonal estimation.
#'
#' \enumerate{
#' \item Extremes in the seasonal-irregular component are currently corrected only in iterations B and C, where the moving irregular standard deviation is always calculated
#' from 5-year moving windows. That is, no final replacement values (D9) are currently computed.
#'
#' \item Users must specify seasonal filters \code{seas_s0} and \code{seas_s1} that match the length of \code{series}, as there
#' is currently no automatic resetting of ill-specified seasonal filters. That is, any specification of seasonal filters that are too long for the given \code{series} will simply produce an error message.
#' }
#'
#' @references Dagum, E. B. and S. Bianconcini (2008). The Henderson Smoother in Reproducing Kernel Hilbert Space. Journal of Business and Economic Statistics 26 (4), 536--545. \url{https://doi.org/10.1198/073500107000000322}
#'
#' Gasser, T. and H.-G. Müller (1979). Kernel Estimation of Regression Functions. In T. Gasser and M. Rosenblatt (Eds), Smoothing Techniques for Curve Estimation, 23--68. Heidelberg: Springer. \url{https://doi.org/10.1007/BFb0098489}
#'
#' Grun-Rehomme, M., F. Guggemos and D. Ladiray (2018). Asymmetric Moving Averages Minimizing Phase Shift. In G. L. Mazzi, D. Ladiray and D. A. Riester (Eds), Handbook on Seasonal Adjustment, 391--413.
#' Luxembourg: Publications Office of the European Union.
#'
#' Proietti, T. and A. Luati (2008). Real Time Estimation in Local Polynomial Regression, with Application to Trend-Cycle Analysis. Annals of Applied Statistics 2 (4), 1523--1553. \url{https://doi.org/10.1214/08-AOAS195}
#'
#' Webel, K. (2026). Some Thoughts on Modified X-11 Seasonal Adjustments for Time Series with Complex Seasonality. Deutsche Bundesbank Discussion Paper XX/2026. Forthcoming
#'
#' Webel, K. and A. Smyk (2024). Seasonal Adjustment of Infra-Monthly Time Series with JDemetra+. Journal of Official Statistics 40 (4), 783--828. \url{https://doi.org/10.1177/0282423X241277602}
#'
#' @return An object of class \code{"hf_decomposition"}. It contains the specified parameters and a matrix that stores the input \code{series} and the final estimates of the seasonally adjusted series (D11),
#' the trend-cyclical component (D12), the seasonal component (D10), and the irregular component (D13).
#' @export
#'
#' @examples
#' x11decomp  <- x11plus(
#' series = rjd3toolkit::ABS$X0.2.09.10.M,
#' period = 12,
#' trend_horizon = 99
#' )

x11plus <- function(series,
                    period,
                    multiplicative = TRUE,
                    trend_horizon = 6,
                    trend_degree = 3,
                    trend_kernel = c(
                      "Henderson",
                      "BiWeight",
                      "TriWeight",
                      "TriCube",
                      "Uniform",
                      "Triangular",
                      "Epanechnikov",
                      "Trapezoidal"
                    ),
                    trend_asymmetric = c("CutAndNormalize", "Direct", "MMSRE"),
                    trend_coefs,
                    seas_s0 = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                    seas_s1 = c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                    extreme_lsig = 1.5,
                    extreme_usig = 2.5,
                    user_defined = NULL) {

  if (missing(period)) {
    if (inherits(series, "ts")) {
      period = stats::frequency(series)
    } else {
      stop("period has to be specified, if series is not of class ts")
    }
  }

    use_trend_coefs <- !missing(trend_coefs)

  if (period < 2) {
    stop("Invalid period: must be greater than or equal to 2.")
  }

  if (extreme_lsig < 0) {
    stop("Invalid extreme_lsig: must be non-negative.")
  }

  if (extreme_usig <= extreme_lsig) {
    stop("Invalid extreme_usig: must be greater than extreme_lsig.")
  }

   if (use_trend_coefs) {
    result <- .x11plus_trend(
      series,
      period,
      trend_coefs,
      multiplicative,
      seas_s0,
      seas_s1,
      extreme_lsig,
      extreme_usig,
      user_defined
    )
    return(result)
  } else {
    result <- .x11plus_default(
      series,
      period,
      multiplicative,
      trend_horizon,
      trend_degree,
      match.arg(trend_kernel),
      match.arg(trend_asymmetric),
      seas_s0,
      seas_s1,
      extreme_lsig,
      extreme_usig,
      user_defined
    )
    return(result)
  }
}

#' Perform an X-11 like decomposition with any (non-integer) periodicity.
#'
#' @param series input time-series.
#' @param period Period of the seasonal component, any positive real number.
#' @param multiplicative Boolean indicating if the decomposition mode is multiplicative (TRUE).
#' @param trend_horizon bandwidth of trend filters.
#' @param trend_degree polynomial order in local trend model.
#' @param trend_kernel kernel weights in objective function.
#' @param trend_asymmetric truncation type for symmetric filter.
#' \code{"matrix"}, \code{"lp_filter"} or \code{"rkhs_filter"}.
#' @param seas_s0 Seasonal filter for B5, C5, D5.
#' @param seas_s1 seasonal filter for B10, C10, D10.
#' @param extreme_lsig lower boundary used for outlier correction in irregular.
#' @param extreme_usig upper boundary used for outlier correction in irregular.
#' @param user_defined a vector containing the additional output variables.
#'
#' @details
#' If trend_coefs are provided, the other trend-settings are ignored and .x11plus_trend() is run.
#'
#'
#' @return An object of the class 'hf_decomposition', containing the decomposition and the parameters
#' @keywords internal
#'
#' @examples
#' q <- x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)

.x11plus_default <- function(series,
                             period,
                             multiplicative = TRUE,
                             trend_horizon = 6,
                             trend_degree = 3,
                             trend_kernel = c(
                               "Henderson",
                               "BiWeight",
                               "TriWeight",
                               "TriCube",
                               "Uniform",
                               "Triangular",
                               "Epanechnikov",
                               "Trapezoidal"
                             ),
                             trend_asymmetric = c("CutAndNormalize", "Direct", "MMSRE"),
                             seas_s0 = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                             seas_s1 = c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                             extreme_lsig = 1.5,
                             extreme_usig = 2.5,
                             user_defined = NULL) {



  seas0 <- match.arg(seas_s0)
  seas1 <- match.arg(seas_s1)
  tkernel <- match.arg(trend_kernel)
  asym <- match.arg(trend_asymmetric)
  jrslt <- rJava::.jcall(
    "jdplus/x12plus/base/r/X11Decomposition",
    "Ljdplus/x12plus/base/r/X11Decomposition$Results;",
    "process",
    as.numeric(series),
    as.numeric(period),
    multiplicative,
    as.integer(trend_horizon),
    as.integer(trend_degree),
    tkernel,
    asym,
    seas0,
    seas1,
    extreme_lsig,
    extreme_usig
  )

  decomposition <- data.frame(
    series = as.numeric(series),
    sa = rjd3toolkit::.proc_vector(jrslt, "d11"),
    t = rjd3toolkit::.proc_vector(jrslt, "d12"),
    s = rjd3toolkit::.proc_vector(jrslt, "d10"),
    i = rjd3toolkit::.proc_vector(jrslt, "d13")
  )

  parameters <- list(
    period = period,
    multiplicative = multiplicative,
    trend_horizon = trend_horizon,
    trend_degree = trend_degree,
    trend_kernel = trend_kernel,
    trend_asymmetric = trend_asymmetric,
    extreme_lsig = extreme_lsig,
    extreme_usig = extreme_usig,
    seas_s0 = seas_s0,
    seas_s1 = seas_s1
  )

  res <- structure(
    list(decomposition = decomposition, parameters = parameters),
    class = c("hf_decomposition")
  )

  res$user_defined <- rjd3toolkit::user_defined(rjd3toolkit::.jd3_object(jrslt, result = TRUE),
                                                userdefined = user_defined)

  return(res)
}

#' X-11 Decomposition With Custom Trend Filters
#'
#' Perform the X-11 decomposition using custom trend filter
#' @param series input time-series.
#' @param period period.
#' @param trend_coefs coefficients of the filters used for the trend-cycle extraction from
#' the real-time asymmetric filter to the symmetric filter. Can be a, object of class \code{"list"},
#' \code{"matrix"}, \code{"lp_filter"} or \code{"rkhs_filter"}.
#' @param multiplicative boolean indicating if the decomposition mode is multiplicative.
#' @param seas_s0,seas_s1 seasonal filters.
#' @param extreme_lsig,extreme_usig boundaries used for outlier correction in irregular.
#' @param user_defined a vector containing the additional output variables.
#' @keywords internal

.x11plus_trend <- function(series,
                           period = stats::frequency(series),
                           trend_coefs,
                           multiplicative = TRUE,
                           seas_s0 = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                           seas_s1 = c("S3X5", "S3X3", "S3X1", "S3X9", "S3X15"),
                           extreme_lsig = 1.5,
                           extreme_usig = 2.5,
                           user_defined = NULL) {


  seas_s0 <- match.arg(toupper(seas_s0)[1], choices = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"))
  seas_s1 <- match.arg(toupper(seas_s1)[1], choices = c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"))

  if (!inherits(trend_coefs, "finite_filters")) {
    stop("The trend_coefs need to be of class finite_filters. Use the rjd3filters package to achieve this.")
  }

  sym_filter <- trend_coefs@sfilter
  asy_filter <- trend_coefs@rfilters
  right_trend_filter <- do.call(cbind, lapply(asy_filter, function(x) {
    c(stats::coef(x), rep(0, 2 * length(asy_filter) - length(x)))
  }))
  if (length(sym_filter) != 2 * ncol(right_trend_filter) + 1) {
    stop(sprintf("The symmetric filter is of length %i but only %i asymmetric filters provided",
                 length(sym_filter),
                 2 * ncol(right_trend_filter) + 1))
  }

  rtrendf <- rjd3toolkit::.r2jd_matrix(right_trend_filter)

  ctrendf <- stats::coef(sym_filter)

  jrslt <- rJava::.jcall("jdplus/x12plus/base/r/X11Decomposition",
    "Ljdplus/x12plus/base/r/X11Decomposition$Results;",
    "trendX11",
    as.numeric(series),
    as.numeric(period),
    multiplicative,
    ctrendf,
    rtrendf,
    seas_s0,
    seas_s1,
    extreme_lsig,
    extreme_usig
  )

  decomposition <- data.frame(series,
                              rjd3toolkit::.proc_vector(jrslt, "d11"),
                              rjd3toolkit::.proc_vector(jrslt, "d10"),
                              rjd3toolkit::.proc_vector(jrslt, "d12"),
                              rjd3toolkit::.proc_vector(jrslt, "d13"))
  colnames(decomposition) <- c("series", "sa", "s", "t", "i")
  res <- structure(list(decomposition = decomposition),
    class = c(class = "hf_decomposition"
    )
  )

  res$parameters <- list(
      period = period,
      multiplicative = multiplicative,
      trend_coefs = trend_coefs,
      extreme_lsig = extreme_lsig,
      extreme_usig = extreme_usig,
      seas_s0 = seas_s0,
      seas_s1 = seas_s1
  )

  res$user_defined <- rjd3toolkit::user_defined(rjd3toolkit::.jd3_object(jrslt, result = TRUE),
                                                  userdefined = user_defined)

  return(res)
}


#' Apply Henderson linear filter
#'
#' @param series Input time series.
#' @param length Length of the symmetric Henderson filter. Must be odd.
#' @param musgrave Boolean indicating if Musgrave asymmetric filters should be used. Default is \code{TRUE}. If \code{FALSE}, no asymmetric filters will be used.
#' @param ic I/C ratio between the volatility of the tentative irregular and trend-cycle estimates (needed for calculating the Musgrave asymmetric filters).
#'
#' @return A numeric array corresponding to the estimated trend.
#' @export
#'
#' @examples
#' q <- x11plus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#'
#' henderson(q$decomposition$sa, 13)

henderson <- function(series,
                      length,
                      musgrave = TRUE,
                      ic = 4.5) {
  result <- rJava::.jcall(
    "jdplus/x12plus/base/r/X11Decomposition",
    "[D",
    "henderson",
    as.numeric(series),
    as.integer(length),
    musgrave,
    ic
  )
  if (stats::is.ts(series))
    result <- stats::ts(result, start = stats::start(series), frequency = stats::frequency(series))

  return(result)
}
