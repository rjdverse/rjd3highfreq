#' Create a specification for the Extended Airline model
#'
#' This internal function constructs a Java ExtendedAirlineSpec object.
#' The Extended Airline model is an extension of the classic seasonal ARIMA
#' model that can handle multiple simultaneous periods.
#'
#' @param period Numeric vector of periods present in the data.
#'   For example, \code{c(7, 365.25)} indicates weekly and annual seasonality.
#' @param differencing Differencing order to apply. The default value
#'   \code{-1} activates automatic computation based on the number of
#'   periods: if \code{ar=FALSE}, the order will be
#'   \code{length(period) + 1}, otherwise it will equal
#'   \code{length(period)}.
#'   Positive values manually specify the differencing order.
#' @param ar Logical. If \code{TRUE}, uses a regular stationary autoregressive
#'   (AR) polynomial instead of a moving average (MA) polynomial.
#'   Default: \code{FALSE}. This choice affects the automatic differencing order.
#' @param to_int Logical. If \code{TRUE}, rounds periodicity values
#'   to integers before processing. Default: \code{FALSE}.
#'
#' @return A Java object of class \code{ExtendedAirlineSpec}.
#' @keywords internal

.extended_airline_spec <- function(period,
                                   differencing = -1,
                                   ar = FALSE,
                                   to_int = FALSE) {
  if (differencing == -1) {
    differencing <- length(period)
    if (!ar)
      differencing <- differencing + 1

  }

  jrslt <- rJava::.jcall(
    "jdplus/highfreq/base/r/ExtendedAirlineProcessor",
    "Ljdplus/highfreq/base/api/ExtendedAirlineSpec;",
    "spec",
    rJava::.jarray(as.numeric(period)),
    as.integer(differencing),
    as.logical(ar),
    as.logical(to_int)
  )
  return(jrslt)
}

#' Create a RegARIMA model based on an Extended Airline specification
#'
#' This internal function constructs a Java RegArimaModel object by fitting an
#' Extended Airline model.
#' @param series time series
#' @param jspec A Java ExtendedAirlineSpec object, for instance created using
#'   \code{\link{.extended_airline_spec}}.
#' @param mean Logical. If \code{TRUE}, includes a mean correction term in the model.
#'   Default: \code{FALSE}.
#' @param xreg Optional matrix of regression variables.
#'   Default: \code{NULL} (no regressors).
#' @return A Java object of class \code{RegArimaModel} from the JDemetra+ toolkit.
#' @keywords internal

.extended_airline_regarima <- function(series, jspec, mean = FALSE, xreg = NULL) {
  jrslt <- rJava::.jcall(
    "jdplus/highfreq/base/r/ExtendedAirlineProcessor",
    "Ljdplus/toolkit/base/core/regarima/RegArimaModel;",
    "regarima",
    as.numeric(series),
    as.logical(mean),
    rjd3toolkit::.r2jd_matrix(xreg),
    jspec
  )
  return(jrslt)
}

#' Estimate parameters of an Extended Airline RegARIMA model
#'
#' This internal function performs maximum likelihood estimation of an Extended
#' Airline model that has been previously specified and initialized.
#'
#' @param jregarima A Java RegArimaModel object, for instance created using
#'  \code{\link{.extended_airline_regarima}}.
#' @param jspec A Java ExtendedAirlineSpec object, for instance created using
#'  \code{\link{.extended_airline_spec}}.
#' @param eps Numeric scalar specifying the convergence tolerance for the
#'   optimization algorithm.
#' @param deps Numeric scalar. Step in the computation of the numerical derivatives,
#'   used in the optimisation routine. Default:1e-4.
#' @param exact_hessian Logical. If \code{TRUE}, computes the exact Hessian matrix
#'   at the optimum for calculating standard errors. If \code{FALSE} (default),
#'   uses a numerical approximation.
#' @return A list object containing detailed estimation results.
#' @keywords internal

.extended_airline_estimation <- function(jregarima,
                                         jspec,
                                         eps = 1e-9,
                                         deps = 1e-9,
                                         exact_hessian = FALSE) {
  jrslt <- rJava::.jcall(
    "jdplus/highfreq/base/r/ExtendedAirlineProcessor",
    "Ljdplus/highfreq/base/core/extendedairline/LightExtendedAirlineEstimation;",
    "estimate",
    jregarima,
    jspec,
    as.numeric(eps),
    as.numeric(deps),
    as.logical(exact_hessian)
  )
  return(rjd3toolkit::.jd3_object(jrslt, result = TRUE)$internal)
}

#' Estimate an Extended Airline RegARIMA model
#'
#' This function combines the three main Extended Airline steps:
#'
#' 1. create an Extended Airline specification,
#' 2. initialise a RegARIMA model,
#' 3. estimate the model parameters.
#'
#' The Java estimation result is post-processed so that selected vector
#' components, such as the input series and full residuals, are returned as
#' regular R vectors.
#'
#' @param series Numeric vector containing the time series to be modelled.
#' @param period Numeric vector of periods present in the data.
#'   For example, \code{c(7, 365.25)} for weekly and annual seasonality.
#' @param differencing Differencing order to apply. The default value
#'   \code{-1} activates automatic computation.
#' @param ar Logical. If \code{TRUE}, uses a regular stationary AR polynomial
#'   instead of an MA polynomial. Default is \code{FALSE}.
#' @param to_int Logical. If \code{TRUE}, rounds periodicity values to integers.
#'   Default is \code{FALSE}.
#' @param mean Logical. If \code{TRUE}, includes a mean correction term in the
#'   RegARIMA model. Default is \code{FALSE}.
#' @param xreg Optional matrix of regression variables. Default is \code{NULL}.
#' @param eps Numeric scalar specifying the convergence tolerance for the
#'   optimisation algorithm. Default is \code{1e-9}.
#' @param deps Numeric scalar used for the computation of numerical derivatives.
#'   Default is \code{1e-9}.
#' @param exact_hessian Logical. If \code{TRUE}, computes the exact Hessian at the
#'   optimum. Default is \code{FALSE}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{spec}}{The Java ExtendedAirlineSpec object.}
#'   \item{\code{regarima}}{The initialised Java RegArimaModel object.}
#'   \item{\code{estimation}}{A formatted R list containing extracted estimation components.}
#'   \item{\code{estimation_raw}}{The original raw Java estimation object.}
#' }
#'
#' @keywords internal

.estimate_extended_airline <- function(series,
                                      period,
                                      differencing = -1,
                                      ar = FALSE,
                                      to_int = FALSE,
                                      mean = FALSE,
                                      xreg = NULL,
                                      eps = 1e-9,
                                      deps = 1e-9,
                                      exact_hessian = FALSE) {
  if (missing(series) || length(series) == 0) {
    stop("`series` must be a non-empty numeric vector.", call. = FALSE)
  }

  if (missing(period) || length(period) == 0) {
    stop("`period` must be a non-empty numeric vector.", call. = FALSE)
  }

  jspec <- .extended_airline_spec(
    period = period,
    differencing = differencing,
    ar = ar,
    to_int = to_int
  )

  jregarima <- .extended_airline_regarima(
    series = series,
    jspec = jspec,
    mean = mean,
    xreg = xreg
  )

  estimation_raw <- .extended_airline_estimation(
    jregarima = jregarima,
    jspec = jspec,
    eps = eps,
    deps = deps,
    exact_hessian = exact_hessian
  )


  estimation <- list(
    parameters = c(
        periods=rjd3toolkit::.proc_vector(estimation_raw,"model.periods"),
        mean = rjd3toolkit::.proc_bool(estimation_raw, "mean"),
        differencing=rjd3toolkit::.proc_int(estimation_raw,"model.differencing"),
        ar=rjd3toolkit::.proc_bool(estimation_raw,"model.ar")
        ),
    estimates=c(
        phi=rjd3toolkit::.proc_numeric(estimation_raw,"model.phi"),
        theta=rjd3toolkit::.proc_numeric(estimation_raw,"model.theta"),
        btheta=rjd3toolkit::.proc_vector(estimation_raw,"model.btheta")
        ),
    variance_covariance=rjd3toolkit::.proc_matrix(estimation_raw,"pcov"), # coefficientsCovariance
    missing = rbind(missing_position=rjd3toolkit::.proc_vector(estimation_raw,"missing_pos"),
                   missing_value=rjd3toolkit::.proc_vector(estimation_raw,"missing_val"),
                   missing_stdev=rjd3toolkit::.proc_vector(estimation_raw,"missing_stdev")
    ),
    likelihood = c(loglikelihood = rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.ll"),
                      adj_loglikelihood=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.adjustedll"),
                      ssqerr=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.ssqerr"),
                      nparams=rjd3toolkit::.proc_int(estimation_raw,"likelihood.nparams"),
                      n_obs=rjd3toolkit::.proc_int(estimation_raw,"likelihood.nobs"),
                      n_effectiveobs=rjd3toolkit::.proc_int(estimation_raw,"likelihood.neffectiveobs"),
                      df=rjd3toolkit::.proc_int(estimation_raw,"likelihood.df")
                      ),
    information_criteria = c(
        aic=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.aic"),
        aicc=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.aicc"),
        bic=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.bic"),
        bicc=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.bicc"),
        bic2=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.bic2"),
        hannanquinn=rjd3toolkit::.proc_numeric(estimation_raw,"likelihood.hannanquinn")
    )
    )
  out <- structure(
    list(
      estimation = estimation,
      spec = jspec,
      regarima = jregarima,
      data =  list(
          series = rjd3toolkit::.proc_vector(estimation_raw, "y"),
          xreg = xreg,
          residuals = rjd3toolkit::.proc_vector(estimation_raw, "fullresiduals")
      )
    ),
    class = "extended_airline"
  )

  return(out)
}

#' Log-Level test for Extended Airline Models
#'
#' This function performs a statistical test to determine whether data
#' should be transformed to logarithmic scale or kept in level (original scale)
#'
#' @param input time series
#' @param precision Numeric value specifying the tolerance for convergence
#'   of optimization algorithms. Default: 1e-5.
#' @param deps Numeric scalar. Step in the computation of the numerical derivatives,
#'   used in the optimisation routine. Default:1e-4.
#' @return An object containing the log-level test results. First the AICc of the model in levels and then the AICc of the model with logs
#' @export
#'
#' @examples
#' series <- rjd3toolkit::Retail$BookStores
#'
#' loglevel(series)

loglevel <- function(input,
                     precision = 1e-5,
                     deps = 1e-4) {

    if (inherits(input, "ts")) {
        input <- .estimate_extended_airline(input, period = stats::frequency(input))
    } else {
        stop("Input must be a time series or the output from .estimate_extended_airline()")
    }

    result <- rJava::.jcall(
        "jdplus/highfreq/base/r/ExtendedAirlineProcessor",
        "[D",
        "logLevelTest",
        input$regarima,
        input$spec,
        as.numeric(precision),
        as.numeric(deps)
    )

    names(result) <- c("AICc_level", "AICc_log")
    result
}

#' Outlier Detection for Extended Airline Models
#'
#' This internal function performs automatic outlier detection in high-frequency
#' time series using Extended Airline models.
#'
#' @param input time series
#' @param types Character vector specifying the types of outliers to detect.
#'   Default: c("AO") (additive outliers only). Common options include:
#'   \itemize{
#'     \item "AO" - Additive Outlier
#'     \item "LS" - Level Shift
#'     \item "WO" - Switch Outlier
#'   }
#' @param start Integer specifying the starting position (1-based R indexing) for
#'   outlier detection. Default: 0 (detection from the beginning of the series).
#' @param end Integer specifying the ending position (1-based R indexing) for
#'   outlier detection. Default: 0 (detection until the end of the series).
#' @param critical_value Numeric value for the critical value threshold.
#'   Uses the maximum value among the specified value and a global max-t
#'   threshold based on extreme-value theory, roughly of order sqrt(2 * log(n))).
#'   Default: 0 (global max-t threshold is used).
#' @param max_outliers Integer specifying the maximum number of outliers to detect.
#'   Default: 30.
#' @param max_round Integer specifying the maximum number of detection iterations.
#'   Default: 30.
#' @param precision Numeric value specifying the tolerance for convergence
#'   of optimization algorithms. Default: 1e-5.
#' @param deps Numeric scalar. Step in the computation of the numerical derivatives,
#'   used in the optimisation routine. Default:1e-4.
#' @return A numeric matrix with dimensions [number of outliers detected × 2].
#' @references Outlier critical value using the Ljung algorithm as given in
#' Ljung, G. M. (1993). On outlier detection in time series.
#' Journal of Royal Statistical Society B 55, 559-567.
#' Solution proposed by Brian Monsell (LBS), January 2022
#' @export
#'
#' @examples
#'
#' series <- rjd3toolkit::Exports$Malta
#' find_outliers(series, critical_value = 3.5, types = c("LS", "WO"))

find_outliers <- function(input,
                          types = c("AO"),
                          start = 0,
                          end = 0,
                          critical_value = 0,
                          max_outliers = 30,
                          max_round = 30,
                          precision = 1e-5,
                          deps = 1e-4) {
    if (start != 0) start <- start - 1
    if (end != 0) end <- end - 1

    if (inherits(input, "ts")) {
        input <- .estimate_extended_airline(input, period = stats::frequency(input))
    } else {
        stop("Input must be a time series or the output from .estimate_extended_airline()")
    }

    result <- rJava::.jcall(
        "jdplus/highfreq/base/r/ExtendedAirlineProcessor",
        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
        "outliers",
        input$regarima,
        input$spec,
        rJava::.jarray(tolower(types)),
        as.integer(start),
        as.integer(end),
        as.numeric(critical_value),
        as.integer(max_outliers),
        as.integer(max_round),
        as.numeric(precision),
        as.numeric(deps)
    )

    out <- rjd3toolkit::.jd2r_matrix(result) + 1

    # Translate numeric codes in the second column to type names
    translate_outlier_types <- function(out, types) {
        column_names <- c("index_of_occurence", "outlier_type")

        # Handle NULL or completely empty output
        if (is.null(out) || length(out) == 0) {
            return(data.frame(
                index_of_occurence = integer(0),
                outlier_type = character(0),
                stringsAsFactors = FALSE
            ))
        }

        out <- as.data.frame(out, stringsAsFactors = FALSE)

        if (ncol(out) < 2) {
            stop("'out' must have at least two columns.")
        }

        # Handle data.frame / matrix with zero rows
        if (nrow(out) == 0) {
            names(out)[1:2] <- column_names
            out[[2]] <- character(0)
            return(out)
        }

        type_index <- suppressWarnings(as.integer(out[[2]]))

        if (any(is.na(type_index))) {
            stop("The outlier type column contains values that cannot be converted to integers.")
        }

        if (any(type_index < 1L | type_index > length(types))) {
            stop("The outlier type column contains values outside the range of 'types'.")
        }

        out[[2]] <- toupper(types[type_index])

        names(out)[1:2] <- column_names

        return(out)
    }

    out <- translate_outlier_types(out, types)

    return(out)
}
