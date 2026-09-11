#' Perform an Arima Model Based (AMB) decomposition
#'
#' Performs an Arima Model Based (AMB) decomposition using a (fractional)
#' airline model, suitable for high-frequency time series. The method
#' decomposes the input series into trend, seasonal and irregular components,
#' with optional signal–noise decomposition.
#'
#' @param series input time series.
#' @param period period of the seasonal component, any positive real number.
#' @param sn decomposition into signal and noise (2 components only). The signal
#'   is the seasonally adjusted series and the noise the seasonal component.
#'   Default: FALSE.
#' @param stde Boolean: TRUE: compute standard deviations of the components.
#'   In some cases (e.g. memory limits), it is currently not possible to compute
#'   them. Default: FALSE.
#' @param nbcasts number of backcasts. Default: 0.
#' @param nfcasts number of forecasts. Default: 0.
#' @param eps precision of the optimisation routine. Default:1e-9.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param log logical indicating whether the series is on the log scale.
#'   Default: FALSE.
#' @param series_time vector of times at which `series` is indexed. Optional.
#'
#' @return An object containing the AMB decomposition results, including the
#'   estimated components and, if requested, their standard deviations.
#'
#' @details
#' If `sn = TRUE`, the decomposition is restricted to two components only
#' (signal and noise). When `stde = TRUE`, the computation of standard deviations
#' may fail for long series or high-frequency data due to memory constraints.
#'
#' @export
#'
#' @examples
#' series <- rnorm(70)+100
#'
#' ### Example with a daily time series with a day-of-the-week effect
#' weekday <- rjd3highfreq::fractional_airline_decomposition(
#'   series,
#'   period = 7,
#'   log = TRUE,
#'   series_time = seq.Date(from=as.Date("2025-01-01"),
#'                          by = "days",
#'                          length.out = length(series))
#' )
#'
#'### Example with a weekly time series
#' series <-  rnorm(200)+100
#' weekly <- rjd3highfreq::fractional_airline_decomposition(
#'   series,
#'   period = 52.18,
#'   log =  TRUE,
#'   series_time = seq.Date(from=as.Date("2025-01-01"),
#'                          by = "days",
#'                          length.out = length(series))
#' )

fractional_airline_decomposition <- function(series,
                                             period,
                                             sn = FALSE,
                                             stde = FALSE,
                                             nbcasts = 0,
                                             nfcasts = 0,
                                             eps = 1e-9,
                                             deps = 1e-4,
                                             log = FALSE,
                                             series_time = NULL) {

  if (!is.numeric(series)) {
    stop("series must be numeric.", call. = FALSE)
  }
  if (!is.numeric(period) || length(period) != 1L) {
    stop("period must be a numeric value of length 1.", call. = FALSE)
  }
  if (!is.logical(sn) || length(sn) != 1L) {
    stop("sn must be a logical value of length 1.", call. = FALSE)
  }

  jrslt <- rJava::.jcall(
    "jdplus/highfreq/base/r/FractionalAirlineProcessor",
    "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
    "decompose",
    as.numeric(series),
    as.numeric(period),
    sn,
    stde,
    as.integer(nbcasts),
    as.integer(nfcasts),
    as.numeric(eps),
    as.numeric(deps)
  )

  out <- .jd2r_fractional_airline_decomposition(jrslt,
                                                sn,
                                                stde,
                                                period,
                                                log,
                                                series_time)
  out$parameters <- c(
      out$parameters,
      nbcasts = nbcasts,
      nfcasts = nfcasts,
      eps = eps,
      deps = deps,
      stde = stde
  )

  return(out)
}


#' Perform an Arima Model Based (AMB) decomposition with multiple periodicities
#'
#' This function performs an AMB decomposition based on (fractional) airline
#' models allowing for multiple seasonal periodicities at once. It is intended
#' for high-frequency time series where more than one seasonal cycle may be
#' present (e.g. weekly and annual effects).
#'
#' If a single period is supplied, the function falls back to
#' \code{fractional_airline_decomposition()}.
#'
#' @param series input time series.
#' @param period numeric vector of seasonal periods. Each value must be a
#'   positive real number (e.g. 7 for weekly, 365.2425 for annual seasonality).
#' @param ndiff integer specifying the number of regular differences.
#'   Default is 2.
#' @param ar logical. If TRUE, an autoregressive component is included in the
#'   model. Default is FALSE.
#' @param stde logical. If TRUE, compute standard deviations of the components.
#'   In some cases (e.g. memory limits), it may not be possible to compute them.
#'   Default is FALSE.
#' @param nbcasts number of backcasts. Default is 0.
#' @param nfcasts number of forecasts. Default is 0.
#' @param eps precision of the optimisation routine. Default: 1e-9.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param log logical. If TRUE, the decomposition is returned on the log-scale.
#'   Default is FALSE.
#' @param series_time optional vector of time indices associated with \code{series}.
#'
#' @return
#' A decomposition object containing the estimated components for each
#' periodicity. If multiple periods are provided, a multi-period decomposition
#' is returned.
#'
#' @export
#'
#' @examples
#' series <- rnorm(200)+100
#' dual_season <- multi_airline_decomposition(
#'   series,
#'   period = c(7, 30.4),
#'   log = TRUE,
#'   series_time = seq.Date(from=as.Date("2025-01-01"),
#'                          by = "days",
#'                          length.out = length(series))
#' )


multi_airline_decomposition <- function(series,
                                        period,
                                        ndiff = 2,
                                        ar = FALSE,
                                        stde = FALSE,
                                        nbcasts = 0,
                                        nfcasts = 0,
                                        eps = 1e-9,
                                        deps = 1e-4,
                                        log = FALSE,
                                        series_time = NULL) {
  if (length(period) == 1) {
    return(fractional_airline_decomposition(series,
                                            period,
                                            stde = stde,
                                            nbcasts = nbcasts,
                                            nfcasts = nfcasts,
                                            eps = eps,
                                            deps = deps,
                                            log = log,
                                            series_time = series_time))
  }
  if (!is.numeric(series)) {
    stop("series must be numeric.", call. = FALSE)
  }
  jrslt <- rJava::.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                         "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                         "decompose",
                         as.numeric(series),
                         rJava::.jarray(period),
                         as.integer(ndiff),
                         ar,
                         stde,
                         as.integer(nbcasts),
                         as.integer(nfcasts),
                         as.numeric(eps),
                         as.numeric(deps))
  if (length(period) == 1) {
    return(.jd2r_fractional_airline_decomposition(jrslt, sn = FALSE,
                                                  stde,
                                                  period,
                                                  log = log,
                                                  series_time = series_time))
  } else {
    return(.jd2r_multi_airline_decomposition(jrslt,
                                             stde,
                                             period,
                                             log = log,
                                             series_time = series_time))
  }
}




#' Linearize a time series using a fractional airline model
#'
#' This function estimates a (fractional) airline RegARIMA model and returns
#' the linearized series together with regression effects, outlier components,
#' estimation results and likelihood diagnostics. It is typically used as a
#' preprocessing step prior to AMB or UCM-based decompositions.
#'
#' Automatic outlier detection can be enabled by specifying the outlier types
#' and a critical value for the detection threshold.
#'
#' @param series input time series.
#' @param period numeric vector of seasonal periods. Each value must be a
#'   positive real number (e.g. 7 for weekly, 365.2425 for annual seasonality).
#' @param xreg optional matrix of user-defined regression variables (e.g. calendar
#'   regressors built using \code{rjd3toolkit}).
#' @param ndiff integer specifying the number of regular differences.
#'   Default is 2.
#' @param ar logical. If TRUE, an autoregressive component is included in the
#'   model. Default is FALSE.
#' @param mean logical. If TRUE, a mean component is included in the model.
#' Default is FALSE.
#' @param outliers character vector specifying the types of outliers to detect.
#'   Possible values include \code{"AO"}, \code{"LS"} and \code{"WO"}.
#'   Default is \code{NULL} (no automatic outlier detection).
#' @param critical_value numeric. Critical value for automatic outlier detection.
#'   Larger values imply more conservative detection. Default is 6.
#' @param precision numeric. Precision of the likelihood optimization.
#'   Default is \code{1e-12}.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param approximate_hessian logical. If TRUE, compute an approximate Hessian
#'   matrix based on the optimization procedure. Default is FALSE.
#' @param nfcasts number of forecasts. Default is 0.
#' @param log logical. If TRUE, the model is estimated on the log-scale.
#'   Default is FALSE.
#' @param series_time optional vector of time indices associated with \code{series}.
#'
#' @return
#' An object of class \code{"hf_estimation"} containing:
#' \itemize{
#'   \item the original and linearized series,
#'   \item estimated regression effects and outlier components,
#'   \item model parameters and covariance matrices,
#'   \item likelihood and diagnostic information.
#' }
#'
#' @export
#'
#' @examples
#' # Simulated examples with and without regressor
#'set.seed(125)
#'reg <- data.frame(
#'    reg1 = round(runif(1000, min = 0, max = 1))*2-1,
#'  reg2 = round(runif(1000, min = 0, max = 1))*2-1
#')
#'
#'y = 100 + 2*reg$reg1 -2*reg$reg2 + rnorm(nrow(reg))
#'
#' # input data
#'data <- list(
#'    series = y,
#'    date = seq.Date(from = as.Date("2020-01-01"),
#'    by = "day",
#'                    length.out = 1000)
#' )
#'
#' # Linearize the series using weekly and annual periodicities
#' est <- fractional_airline_estimation(
#'    data$series,
#'    period = c(7, 30.4),
#'    log = FALSE,
#'    xreg = reg,
#'    series_time = data$date
#')
#'
#' est
#'
#' est2 <- fractional_airline_estimation(
#'   data$series,
#'   period = c(7, 30.4),
#'   log = FALSE,
#'   series_time = data$date
#')
#'
#' est2


fractional_airline_estimation <- function(series,
                                          period,
                                          xreg = NULL,
                                          ndiff = 2,
                                          ar = FALSE,
                                          mean = FALSE,
                                          outliers = NULL,
                                          critical_value = 6,
                                          precision = 1e-12,
                                          deps = 1e-4,
                                          approximate_hessian = FALSE,
                                          nfcasts = 0,
                                          log = FALSE,
                                          series_time = NULL) {

  # Input checks
  if (!is.numeric(series)) {
    stop("series must be numeric.", call. = FALSE)
  }
  if (!is.numeric(critical_value) || length(critical_value) != 1L) {
    stop("critical_value must be a numeric value of length 1.", call. = FALSE)
  }
  if (!is.numeric(precision) || length(precision) != 1L) {
    stop("precision must be a numeric value of length 1.", call. = FALSE)
  }
  if (!is.logical(mean) || length(logical) != 1L) {
    stop("logical must be a numeric value of length 1.", call. = FALSE)
  }
    xreg_used <- NULL

    if (!is.null(xreg)) {
        xreg_used <- tryCatch(
            as.matrix(xreg),
            error = function(e) {
                stop(
                    "xreg could not be converted to a matrix.",
                    call. = FALSE
                )
            }
        )

        if (!is.numeric(xreg_used)) {
            stop("xreg must be numeric after conversion to a matrix.", call. = FALSE)
        }
    }



  if (is.null(outliers)) {
    joutliers <- rJava::.jnull("[Ljava/lang/String;")
  } else {
    joutliers <- rJava::.jarray(outliers, "java.lang.String")
  }

  jrslt <- rJava::.jcall(
    obj = "jdplus/highfreq/base/r/FractionalAirlineProcessor",
    returnSig = "Ljdplus/highfreq/base/core/extendedairline/ExtendedAirlineEstimation;",
    method = "estimate",
    as.numeric(series),
    log,
    rjd3toolkit::.r2jd_matrix(xreg_used),
    mean,
    rJava::.jarray(period),
    as.integer(ndiff),
    ar,
    joutliers,
    critical_value,
    as.integer(nfcasts),
    precision,
    deps,
    approximate_hessian
  )

  external_variables <- .proc_variable_outlier_names(
    var_out_names = jrslt$getOutliers(),
    n_x = jrslt$getNx()
  )
  reg_mat <- rjd3toolkit::.proc_matrix(rslt = jrslt, name = "regressors")

  if (is.null(series_time) && !is.null(xreg)) {
    series_time <- rownames(xreg)
  }

  if (!is.null(colnames(xreg)) && sum(duplicated(colnames(xreg))) == 0) {
    external_variables[seq_len(ncol(xreg))] <- colnames(xreg)
    # Outliers
    if (!is.null(series_time) && (length(external_variables) - ncol(xreg) > 0)) {
      outliers <- external_variables[-seq_len(ncol(xreg))]
      outliers_type <- substr(outliers, start = 1, stop = 2)
      outliers_date <- series_time[as.integer(substr(outliers,
                                                     start = 4,
                                                     stop = 50))]
      external_variables[-seq_len(ncol(xreg))] <- paste0(outliers_type,
                                                         ".",
                                                         outliers_date)
    }
  } else if (is.null(xreg)
             && !is.null(series_time)
             && length(external_variables) > 0) {
    outliers <- external_variables
    outliers_type <- substr(outliers, start = 1, stop = 2)
    outliers_date <- series_time[as.integer(substr(outliers,
                                                   start = 4,
                                                   stop = 50))]
    external_variables <- paste0(outliers_type, ".", outliers_date)
  }
  if (!is.null(reg_mat) && ncol(reg_mat) > 0) {
    colnames(reg_mat) <- external_variables
  }
  if (!is.null(reg_mat) && nrow(reg_mat) > 0 && !is.null(series_time)) {
    rownames(reg_mat) <- series_time
  }

  model <- list(
    variables = external_variables,
    # "variables " names of variables and outliers
    xreg = reg_mat,
    # "xreg" matrix of regressor (external variables and outliers)
    b = rjd3toolkit::.proc_vector(jrslt, "b"),
    bcov = rjd3toolkit::.proc_matrix(jrslt, "bvar"),
    component_mean = rjd3toolkit::.proc_vector(jrslt, "component_mean"),
    missingOrNegative = rjd3toolkit::.proc_vector(jrslt, "missing")
  )

  decomposition <- list(
    series       = rjd3toolkit::.proc_vector(jrslt, "y"),
    linearized   = rjd3toolkit::.proc_vector(jrslt, "lin"),
    residuals    = rjd3toolkit::.proc_vector(jrslt, "residuals"),
    component_wo = rjd3toolkit::.proc_vector(jrslt, "component_wo"),
    component_ao = rjd3toolkit::.proc_vector(jrslt, "component_ao"),
    component_ls = rjd3toolkit::.proc_vector(jrslt, "component_ls"),
    component_outliers              = rjd3toolkit::.proc_vector(jrslt,
                                                                "component_outliers"),
    component_userdef_reg_variables = rjd3toolkit::.proc_vector(jrslt,
                                                                "component_userdef_reg_variables")
  )

  estimation <- list(
    period = period,
    series_time  = series_time,
    parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
    t = rjd3toolkit::.proc_vector(jrslt, "parameters")/sqrt(diag(rjd3toolkit::.proc_matrix(jrslt, "pcov"))),
    score = rjd3toolkit::.proc_vector(jrslt, "score"),
    covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
    log = rjd3toolkit::.proc_bool(jrslt, "log")
  )

  likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

  return(structure(list(model = model,
                        decomposition = decomposition,
                        estimation = estimation,
                        likelihood = likelihood),
                   class = "hf_estimation"))
}


