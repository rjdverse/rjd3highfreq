#' Seasonal decomposition using STL+
#'
#' Performs an STL-like seasonal decomposition. It can handle missing values and does allow a multiplicative decomposition.
#'
#' @param series Numeric vector. Input time series to be decomposed.
#' @param period Numeric scalar. Seasonal period of the series. For example,
#'   use \code{12} for monthly data with yearly seasonality, \code{4} for
#'   quarterly data, or \code{7} for daily data with weekly seasonality.
#'   In the current implementation, this value is passed to Java as an integer.
#' @param multiplicative Logical. If \code{TRUE}, a multiplicative
#'   decomposition is used. If \code{FALSE}, an additive decomposition is used.
#' @param swindow Integer. Length of the seasonal smoothing window.
#' @param twindow Integer. Length of the trend smoothing window. If set to
#'   \code{0}, the value is selected automatically by the underlying Java
#'   implementation.
#' @param lwindow Integer. Length of the low-pass filter used to remove the
#'   trend from the seasonal component. If set to \code{0}, the value is
#'   selected automatically by the underlying Java implementation.
#' @param sdegree Integer. Degree of the local polynomial used for seasonal
#'   smoothing. Usually \code{0} or \code{1}.
#' @param tdegree Integer. Degree of the local polynomial used for trend
#'   smoothing. Usually \code{0} or \code{1}.
#' @param ldegree Integer. Degree of the local polynomial used for low-pass
#'   smoothing. Usually \code{0} or \code{1}.
#' @param sjump Integer. Number of jumps used in the computation of the
#'   seasonal component. Values greater than zero speed up the computation by
#'   evaluating the smoother at fewer points and interpolating between them.
#' @param tjump Integer. Number of jumps used in the computation of the trend
#'   component.
#' @param ljump Integer. Number of jumps used in the computation of the
#'   low-pass component.
#' @param robust Boolean. Analogue to robust parameter in stats::stl (see
#'   details)
#' @param ninnerloop Integer. Number of inner iterations of the STL algorithm.
#' @param nouterloop Integer. Number of outer iterations used to compute robust
#'   weights. Set to \code{0} to disable robust fitting.
#' @param weight_threshold Numeric scalar in \code{[0, 0.3]}. Threshold used in
#'   the computation of robust weights.
#' @param weight_function Character string specifying the weighting function
#'   used by the LOESS smoothers. One of \code{"biweight"}, \code{"uniform"},
#'   \code{"triangular"}, \code{"epanechnikov"}, \code{"tricube"} or
#'   \code{"triweight"}.
#' @param legacy Logical. If \code{TRUE}, uses the legacy MAD computation of
#'   the underlying implementation. This option is mainly provided for
#'   backward compatibility.
#'
#' @details
#' This function provides an R interface to the JD+ Java implementation of
#' STL decomposition. It decomposes a time series into trend, seasonal and
#' irregular components and returns the result as an object of class
#' \code{"hf_decomposition"}.
#'
#' The returned decomposition contains the following columns:
#'
#' \describe{
#'   \item{\code{series}}{The original input series.}
#'   \item{\code{sa}}{The seasonally adjusted series (trend+irregular for additive, trend*irregular for multiplicative decomposition).}
#'   \item{\code{t}}{The trend component.}
#'   \item{\code{s}}{The seasonal component.}
#'   \item{\code{i}}{The irregular component.}
#'   \item{\code{fit}}{The fitted values from the decomposition (trend+seasonal(s) for additive, trend*seasonal(s) for multiplicative decomposition).}
#'   \item{\code{weights}}{The final robust weights.}
#' }
#'
#' If \code{multiplicative = TRUE}, the decomposition is interpreted as a
#' multiplicative decomposition. If \code{multiplicative = FALSE}, it is
#' interpreted as an additive decomposition.
#'
#' If \code{robust = TRUE}, the parameters nouterloop and ninnerloop are
#' overwritten, so that 15 iterations of the outer loop and one run of the
#' inner loop are completed. If \code{robust = FALSE}, nouterloop is set to 0,
#' and ninnerloop is set to 2.
#'
#' @return An object of class \code{"hf_decomposition"}, consisting of a list
#' with two elements:
#'
#' \describe{
#'   \item{\code{decomposition}}{A \code{data.frame} containing the original
#'   series, the seasonally adjusted series, the trend, seasonal and irregular
#'   components, fitted values and robust weights (see details).}
#'   \item{\code{parameters}}{A list containing the main parameters used for
#'   the decomposition.}
#' }
#'
#' @export
#'
#' @examples
#' decomp <- stlplus(
#'   series = rjd3toolkit::ABS$X0.2.09.10.M,
#'   period = 12
#' )
#'
#' plot(decomp)
#'
#' @seealso \code{\link{plot.hf_decomposition}}

stlplus <- function(series,
                    period,
                    multiplicative = TRUE,
                    swindow = 7,
                    twindow = 0,
                    lwindow = 0,
                    sdegree = 0,
                    tdegree = 1,
                    ldegree = 1,
                    sjump = 0,
                    tjump = 0,
                    ljump = 0,
                    robust = NULL,
                    ninnerloop = 1,
                    nouterloop = 15,
                    weight_threshold = 0.001,
                    weight_function = c("biweight",
                                        "uniform",
                                        "triangular",
                                        "epanechnikov",
                                        "tricube",
                                        "triweight"),
                    legacy = FALSE) {
  weight_function <- match.arg(weight_function)

  if (!is.null(robust)) {

      if (robust) {
          message("The parameter robust was set to TRUE and therefore ninnerloop = 1, nouterloop = 15")
          ninnerloop = 1
          nouterloop = 15
      } else {
          message("The parameter robust was set to FALSE and therefore ninnerloop = 2, nouterloop = 0")
          ninnerloop = 2
          nouterloop = 0
      }
  }

  jrslt <- rJava::.jcall(
    "jdplus/stl/base/r/StlDecomposition",
    "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
    "stl",
    as.numeric(series),
    as.integer(period),
    as.logical(multiplicative),
    as.integer(swindow),
    as.integer(twindow),
    as.integer(lwindow),
    as.integer(sdegree),
    as.integer(tdegree),
    as.integer(ldegree),
    as.integer(sjump),
    as.integer(tjump),
    as.integer(ljump),
    as.integer(ninnerloop),
    as.integer(nouterloop),
    as.numeric(weight_threshold),
    toupper(as.character(weight_function)),
    as.logical(legacy)
  )
  m <- rjd3toolkit::.jd2r_matrix(jrslt)
  m <- as.data.frame(m)

  colnames(m) <- c("series", "sa", "t", "s", "i", "fit", "weights")
  parameters <- list(
    multiplicative = multiplicative,
    swindow = swindow,
    twindow = twindow,
    lwindow = lwindow,
    sdegree = sdegree,
    tdegree = tdegree,
    ldegree = ldegree,
    sjump = sjump,
    tjump = tjump,
    ljump = ljump,
    robust = robust,
    ninnerloop = ninnerloop,
    nouterloop = nouterloop,
    weight_threshold = weight_threshold,
    weight_function = weight_function
  )

  return(structure(list(
    decomposition = m, parameters = parameters
          ),
    class = "hf_decomposition"))
}

#' Simultaneous, multiple seasonal decomposition using MSTL
#'
#' Performs an stlplus-type decomposition for time series with multiple seasonal
#' periods. This function provides an R interface to the JD+ Java implementation.. It simultaneously decomposes a time series into a trend component,
#' several seasonal components, and an irregular component.
#'
#' @param series Numeric vector. Input time series to be decomposed.
#' @param period Numeric vector. Seasonal periods to be modelled. For example,
#'   \code{c(7, 365)} for daily data with weekly and yearly seasonal patterns.
#'   Values are passed to the underlying Java implementation as integers.
#' @param multiplicative Logical. If \code{TRUE}, a multiplicative
#'   decomposition is used. If \code{FALSE}, an additive decomposition is used.
#' @param swindow Optional integer vector. Lengths of the seasonal smoothing
#'   windows, one for each seasonal period. If \code{NULL}, the seasonal
#'   windows are selected automatically by the underlying Java implementation.
#' @param twindow Integer. Length of the trend smoothing window. If set to
#'   \code{0}, the value is selected automatically by the underlying Java
#'   implementation.
#' @param robust Boolean. Analogue to robust parameter in stats::stl (see
#'   details)
#' @param ninnerloop Integer. Number of inner iterations of the MSTL algorithm.
#' @param nouterloop Integer. Number of outer iterations used to compute robust
#'   weights. Set to \code{0} to disable robust fitting.
#' @param nojump Logical. If \code{TRUE}, disables jump-based acceleration in
#'   the smoothing computations. If \code{FALSE}, the underlying implementation
#'   may use jumps to speed up the decomposition.
#' @param weight_threshold Numeric scalar in \code{[0, 0.3]}. Threshold used in
#'   the computation of robust weights.
#' @param weight_function Character string specifying the weighting function
#'   used by the LOESS smoothers. One of \code{"biweight"}, \code{"uniform"},
#'   \code{"triangular"}, \code{"epanechnikov"}, \code{"tricube"} or
#'   \code{"triweight"}.
#'
#' @details
#' The returned decomposition contains the following columns:
#'
#' \describe{
#'   \item{\code{series}}{The original input series.}
#'   \item{\code{sa}}{The seasonally adjusted series (trend+irregular for additive, trend*irregular for multiplicative decomposition).}
#'   \item{\code{t}}{The trend component.}
#'   \item{\code{s<period>}}{One seasonal component for each value supplied in
#'   \code{period}. For example, if \code{period = c(7, 365)}, the output
#'   contains columns \code{s7} and \code{s365}.}
#'   \item{\code{i}}{The irregular component.}
#'   \item{\code{fit}}{The fitted values from the decomposition (trend+seasonal(s) for additive, trend*seasonal(s) for multiplicative decomposition).}
#'   \item{\code{weights}}{The final robust weights.}
#' }
#'
#' If \code{multiplicative = TRUE}, the decomposition is interpreted as a
#' multiplicative decomposition. If \code{multiplicative = FALSE}, it is
#' interpreted as an additive decomposition.
#'
#' If \code{robust = TRUE}, the parameters nouterloop and ninnerloop are
#' overwritten, so that 15 iterations of the outer loop and one run of the
#' inner loop are completed. If \code{robust = FALSE}, nouterloop is set to 0,
#' and ninnerloop is set to 2.
#'
#' @return An object of class \code{"hf_decomposition"}, consisting of a list
#' with two elements:
#'
#' \describe{
#'   \item{\code{decomposition}}{A \code{data.frame} containing the original
#'   series, the seasonally adjusted series, the trend, one seasonal component
#'   for each period, the irregular component, fitted values and robust
#'   weights.}
#'   \item{\code{parameters}}{A list containing the main parameters used for
#'   the decomposition.}
#' }
#'
#' @export
#'
#' @examples
#' q <- mstl(
#'   series = rjd3toolkit::ABS$X0.2.09.10.M,
#'   period = c(12, 19)
#' )
#'
#' plot(q)
#'
#' @seealso \code{\link{stlplus}}, \code{\link{plot.hf_decomposition}}

mstl <- function(series,
                 period,
                 multiplicative = TRUE,
                 swindow = NULL,
                 twindow = 0,
                 robust = NULL,
                 ninnerloop = 1,
                 nouterloop = 15,
                 nojump = FALSE,
                 weight_threshold = 0.001,
                 weight_function = c("biweight",
                                     "uniform",
                                     "triangular",
                                     "epanechnikov",
                                     "tricube",
                                     "triweight")) {
  weight_function <- match.arg(weight_function)

  if (!is.null(robust)) {

      if (robust) {
          message("The parameter robust was set to TRUE and therefore ninnerloop = 1, nouterloop = 15")
          ninnerloop = 1
          nouterloop = 15
      } else {
          message("The parameter robust was set to FALSE and therefore ninnerloop = 2, nouterloop = 0")
          ninnerloop = 2
          nouterloop = 0
      }
  }

  if (is.null(swindow))
    swin <- rJava::.jnull("[I")
  else
    swin <- rJava::.jarray(as.integer(swindow))

  jrslt <- rJava::.jcall(
    "jdplus/stl/base/r/StlDecomposition",
    "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
    "mstl",
    as.numeric(series),
    rJava::.jarray(as.integer(period)),
    as.logical(multiplicative),
    swin,
    as.integer(twindow),
    as.integer(ninnerloop),
    as.integer(nouterloop),
    as.logical(nojump),
    as.numeric(weight_threshold),
    toupper(as.character(weight_function))
  )
  m <- rjd3toolkit::.jd2r_matrix(jrslt)
  m <- as.data.frame(m)

  snames <- paste("s", as.integer(period), sep = "")
  colnames(m) <- c("series", "sa", "t", snames, "i", "fit", "weights")
  parameters <- list(
    multiplicative = multiplicative,
    swindow = swindow,
    twindow = twindow,
    ninnerloop = ninnerloop,
    nouterloop = nouterloop,
    weight_threshold = weight_threshold,
    weight_function = weight_function
  )

  return(structure(list(
    decomposition = m, parameters = parameters
  ), class = "hf_decomposition"))
}

#' Iterative multiple seasonal decomposition using ISTL
#'
#' Performs an iterative stlplus-type decomposition for time series with multiple
#' seasonal periods. This function provides an R interface to the JD+ Java implementation. It decomposes a time series into a trend component,
#' several seasonal components, and an irregular component using an iterative
#' STL-based procedure.
#'
#' @param series Numeric vector. Input time series to be decomposed.
#' @param period Numeric vector. Seasonal periods to be modelled. For example,
#'   \code{c(7, 365)} for daily data with weekly and yearly seasonal patterns.
#'   Values are passed to the underlying Java implementation as integers.
#' @param multiplicative Logical. If \code{TRUE}, a multiplicative
#'   decomposition is used. If \code{FALSE}, an additive decomposition is used.
#' @param swindow Optional integer vector. Lengths of the seasonal smoothing
#'   windows, one for each seasonal period. If \code{NULL}, the seasonal
#'   windows are selected automatically by the underlying Java implementation.
#' @param twindow Optional integer vector. Lengths of the trend smoothing
#'   windows. If \code{NULL}, the trend windows are selected automatically by
#'   the underlying Java implementation.
#' @param robust Boolean. Analogue to robust parameter in stats::stl (see
#'   details)
#' @param ninnerloop Integer. Number of inner iterations of the ISTL algorithm.
#' @param nouterloop Integer. Number of outer iterations used to compute robust
#'   weights. Set to \code{0} to disable robust fitting.
#' @param nojump Logical. If \code{TRUE}, disables jump-based acceleration in
#'   the smoothing computations. If \code{FALSE}, the underlying implementation
#'   may use jumps to speed up the decomposition.
#' @param weight_threshold Numeric scalar in \code{[0, 0.3]}. Threshold used in
#'   the computation of robust weights.
#' @param weight_function Character string specifying the weighting function
#'   used by the LOESS smoothers. One of \code{"biweight"}, \code{"uniform"},
#'   \code{"triangular"}, \code{"epanechnikov"}, \code{"tricube"} or
#'   \code{"triweight"}.
#'
#' @details
#' The returned decomposition contains the following columns:
#'
#' \describe{
#'   \item{\code{series}}{The original input series.}
#'   \item{\code{sa}}{The seasonally adjusted series (trend+irregular for additive, trend*irregular for multiplicative decomposition).}
#'   \item{\code{t}}{The trend component.}
#'   \item{\code{s<period>}}{One seasonal component for each value supplied in
#'   \code{period}. For example, if \code{period = c(7, 365)}, the output
#'   contains columns \code{s7} and \code{s365}.}
#'   \item{\code{i}}{The irregular component.}
#'   \item{\code{fit}}{The fitted values from the decomposition (trend+seasonal for additive, trend*seasonal for multiplicative decomposition).}
#'   \item{\code{weights}}{The final robust weights.}
#' }
#'
#' If \code{multiplicative = TRUE}, the decomposition is interpreted as a
#' multiplicative decomposition. If \code{multiplicative = FALSE}, it is
#' interpreted as an additive decomposition.
#'
#' If \code{robust = TRUE}, the parameters nouterloop and ninnerloop are
#' overwritten, so that 15 iterations of the outer loop and one run of the
#' inner loop are completed. If \code{robust = FALSE}, nouterloop is set to 0,
#' and ninnerloop is set to 2.
#'
#' @return An object of class \code{"hf_decomposition"}, consisting of a list
#' with two elements:
#'
#' \describe{
#'   \item{\code{decomposition}}{A \code{data.frame} containing the original
#'   series, the seasonally adjusted series, the trend, one seasonal component
#'   for each period, the irregular component, fitted values and robust
#'   weights.}
#'   \item{\code{parameters}}{A list containing the main parameters used for
#'   the decomposition.}
#' }
#'
#' @export
#'
#' @examples
#' q <- istl(
#'   series = rjd3toolkit::ABS$X0.2.09.10.M,
#'   period = c(12, 19)
#' )
#'
#' plot(q)
#'
#' @seealso \code{\link{stlplus}}, \code{\link{mstl}},
#'   \code{\link{plot.hf_decomposition}}

istl <- function(series,
                 period,
                 multiplicative = TRUE,
                 swindow = NULL,
                 twindow = NULL,
                 robust = NULL,
                 ninnerloop = 1,
                 nouterloop = 15,
                 nojump = FALSE,
                 weight_threshold = 0.001,
                 weight_function = c("biweight",
                                     "uniform",
                                     "triangular",
                                     "epanechnikov",
                                     "tricube",
                                     "triweight")) {
  weight_function <- match.arg(weight_function)

  if (!is.null(robust)) {

      if (robust) {
          message("The parameter robust was set to TRUE and therefore ninnerloop = 1, nouterloop = 15")
          ninnerloop = 1
          nouterloop = 15
      } else {
          message("The parameter robust was set to FALSE and therefore ninnerloop = 2, nouterloop = 0")
          ninnerloop = 2
          nouterloop = 0
      }
  }

  if (is.null(swindow))
    swin <- rJava::.jnull("[I")
  else
    swin <- rJava::.jarray(as.integer(swindow))

  if (is.null(twindow))
    twin <- rJava::.jnull("[I")
  else
    twin <- rJava::.jarray(as.integer(twindow))

  jrslt <- rJava::.jcall(
    "jdplus/stl/base/r/StlDecomposition",
    "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
    "istl",
    as.numeric(series),
    rJava::.jarray(as.integer(period)),
    as.logical(multiplicative),
    swin,
    twin,
    as.integer(ninnerloop),
    as.integer(nouterloop),
    as.logical(nojump),
    as.numeric(weight_threshold),
    toupper(as.character(weight_function))
  )
  m <- rjd3toolkit::.jd2r_matrix(jrslt)
  m <- as.data.frame(m)

  snames <- paste("s", as.integer(period), sep = "")
  colnames(m) <- c("series", "sa", "t", snames, "i", "fit", "weights")
  parameters <- list(
    multiplicative = multiplicative,
    swindow = swindow,
    twindow = twindow,
    ninnerloop = ninnerloop,
    nouterloop = nouterloop,
    weight_threshold = weight_threshold,
    weight_function = weight_function
  )

  return(structure(list(
    decomposition = m,
    parameters = parameters
  ), class = "hf_decomposition"))
}


#' Fit a LOESS smoother
#'
#' Applies a LOESS smoother to a numeric time series.
#'
#' @param series Numeric vector. Input time series to be smoothed.
#' @param window Integer. Length of the LOESS smoothing window. Larger values
#'   produce a smoother result.
#' @param degree Integer. Degree of the local polynomial. Supported values are
#'   \code{0} for locally constant smoothing and \code{1} for locally linear
#'   smoothing. The default is \code{1}.
#' @param jump Integer. Number of jumps used to speed up the computation.
#'   If \code{jump = 0}, the smoother is evaluated at every observation.
#'   Values greater than zero evaluate the smoother at fewer points and
#'   interpolate between them. The default is \code{0}.
#'
#' @return A numeric vector containing the smoothed series.
#'
#' @details
#' This function provides a simple R interface to the JD+ Java implementation
#' of the LOESS smoother used internally by the stlplus decomposition routines.
#' It smoothes the input series using a local polynomial of degree 0 or 1.
#'
#' This function is not the same as \code{\link[stats]{loess}} from the
#' \pkg{stats} package. It does not use a formula interface and is intended
#' for smoothing a single numeric series with the JD+ stlplus|loess implementation.
#' Also, it can handle time series with missing values.
#'
#' @export
#'
#' @examples
#' q <- stlplus(
#'   series = rjd3toolkit::ABS$X0.2.09.10.M,
#'   period = 12
#' )
#'
#' trend <- q$decomposition[, "t"]
#'
#' smoothed_trend <- loess(
#'   series = trend,
#'   window = 121
#' )
#'
#' ts.plot(
#'   cbind(trend, smoothed_trend),
#'   col = c("black", "red")
#' )
#'
#' @seealso \code{\link{stlplus}}, \code{\link{mstl}},
#'   \code{\link{istl}}, \code{\link[stats]{loess}}


loess <- function(series,
                  window,
                  degree = 1,
                  jump = 0) {
  if (degree != 0 && degree != 1)
    stop("Unsupported degree")
  if (jump < 0)
    stop("jump should be positive")
  return(
    rJava::.jcall(
      "jdplus/stl/base/r/StlDecomposition",
      "[D",
      "loess",
      as.numeric(series),
      as.integer(window),
      as.integer(degree),
      as.integer(jump)
    )
  )
}
