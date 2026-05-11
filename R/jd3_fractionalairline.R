#' @include utils.R
NULL

#' Extract a specific component from a Java Decomposition object
#'
#' This internal function retrieves a single component from a decomposition
#' result object returned by the Java backend.
#' The component can be specified either by name or by its integer code.
#'
#' @param jrslt Java Decomposition object.
#' @param cmp Character string or integer specifying the component to extract.
#'   Valid components are:
#'   \itemize{
#'     \item "Series" or 1 : Complete series (Trend + Seasonal + Irregular + CalendarEffect)
#'     \item "Trend" or 2 : Trend / level component
#'     \item "Seasonal" or 3 : Seasonal component
#'     \item "SeasonallyAdjusted" or 4 : Trend + Seasonal + CalendarEffect
#'     \item "Irregular" or 5 : Irregular / residual component
#'     \item "CalendarEffect" or 6 : Calendar effects (e.g., holidays)
#'   }
#'
#' @return The requested component extracted from the UCM result.
#'
#' @examples
#' \dontrun{
#' # Extract the trend component
#' trend <- .ucm_extract(jrslt, "Trend")
#' }
#' @export
.ucm_extract<-function(jrslt, cmp) {
    path<-paste0("ucarima.component(", cmp,")")
    return(.arima_extract(jrslt, path))
}

#' Extract an ARIMA model from a Java object
#'
#' This internal function retrieves the ARIMA specification associated with a
#' given path in a Java object. It extracts the model
#' structure (description string, AR/MA polynomials, differencing order) and
#' the innovation variance, and returns them as an R `arima_model` object.
#'
#' @param jrslt Java object containing the estimated RegARIMA.
#' @param path Character string specifying the extraction path within the Java
#'   object
#'
#' @return An `arima_model` object as constructed by
#'   `rjd3toolkit::arima_model()`.
#' @export
.arima_extract<-function(jrslt, path) {
    str<-rjd3toolkit::.proc_str(jrslt, paste0(path, ".name"))
    ar<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ar"))
    delta<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".delta"))
    ma<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ma"))
    var<-rjd3toolkit::.proc_numeric(jrslt, paste0(path, ".var"))
    return(rjd3toolkit::arima_model(str, ar,delta,ma,var))
}

#' Perform an Arima Model Based (AMB) decomposition
#'
#' Performs an Arima Model Based (AMB) decomposition using a (fractional)
#' airline model, suitable for high-frequency time series. The method
#' decomposes the input series into trend, seasonal and irregular components,
#' with optional signal–noise decomposition.
#'
#' @param y input time series.
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
#' @param y_time vector of times at which `y` is indexed. Optional.
#'
#' @return An object containing the AMB decomposition results, including the
#'   estimated components and, if requested, their standard deviations.
#'
#' @details
#' If `sn = TRUE`, the decomposition is restricted to two components only
#' (signal and noise). When `stde = TRUE`, the computation of standard deviations
#' may fail for long series or high-frequency data due to memory constraints.
#'
#' @examples
#' amb.dow <- fractionalAirlineDecomposition(
#'   y = Births$births,
#'   period = 7,
#'   log = FALSE, y_time = Births$date)
#'
#' amb.doy <- fractionalAirlineDecomposition(
#'   y = Births$births,
#'   period = 365.2425,
#'   log = FALSE, y_time = Births$date)
#' @export
fractionalAirlineDecomposition <- function(y,
                                           period,
                                           sn = FALSE,
                                           stde = FALSE,
                                           nbcasts = 0,
                                           nfcasts = 0,
                                           eps = 1e-9,
                                           deps=1e-4,
                                           log = FALSE,
                                           y_time = NULL) {
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(period, len = 1, null.ok = FALSE)
    checkmate::assertLogical(sn, len = 1, null.ok = FALSE)
    jrslt <- .jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                    "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                    "decompose", as.numeric(y), as.numeric(period), sn, stde, as.integer(nbcasts),
                    as.integer(nfcasts), as.numeric(eps), as.numeric(deps))
    return(jd2r_fractionalAirlineDecomposition(jrslt, sn, stde, period, log, y_time))
}


#' Perform an Arima Model Based (AMB) decomposition with multiple periodicities
#'
#' This function performs an AMB decomposition based on (fractional) airline
#' models allowing for multiple seasonal periodicities at once. It is intended
#' for high-frequency time series where more than one seasonal cycle may be
#' present (e.g. weekly and annual effects).
#'
#' If a single period is supplied, the function falls back to
#' \code{fractionalAirlineDecomposition()}.
#'
#' @param y input time series.
#' @param periods numeric vector of seasonal periods. Each value must be a
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
#' @param eps precision of the optimisation routine. Default:1e-9.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param log logical. If TRUE, the decomposition is returned on the log-scale.
#'   Default is FALSE.
#' @param y_time optional vector of time indices associated with \code{y}.
#'
#' @return
#' A decomposition object containing the estimated components for each
#' periodicity. If multiple periods are provided, a multi-period decomposition
#' is returned.
#'
#'
#' @examples
#'  # Using the linearized series; decomposition fails on raw (non-linearized)
#'  # birth data.
#'
#' # Weekly and annual periodicities
#' amb.multi <- multiAirlineDecomposition(
#'   y = pre_proc_births$model$linearized,
#'   periods = c(7, 365.2425),
#'   log = FALSE,
#'   y_time = pre_proc_births$model$date
#' )
#'
#' @export
multiAirlineDecomposition <- function(y, periods, ndiff = 2, ar = FALSE, stde = FALSE,
                                      nbcasts = 0, nfcasts = 0, eps = 1e-9, deps=1e-4,
                                      log = FALSE, y_time = NULL) {
    if (length(periods) == 1) {
        return(fractionalAirlineDecomposition(y, periods, stde = stde,
                                              nbcasts = nbcasts, nfcasts = nfcasts, eps = eps, deps = deps,
                                              log = log, y_time = y_time))
    }
    checkmate::assertNumeric(y, null.ok = FALSE)
    jrslt <- .jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                    "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                    "decompose", as.numeric(y), .jarray(periods), as.integer(ndiff),
                    ar, stde, as.integer(nbcasts), as.integer(nfcasts),as.numeric(eps), as.numeric(deps))
    if (length(periods) == 1) {
        return(jd2r_fractionalAirlineDecomposition(jrslt, sn = FALSE, stde, periods,
                                                   log = log, y_time = y_time))
    } else {
        return(jd2r_multiAirlineDecomposition(jrslt, stde, periods,
                                              log = log, y_time = y_time))
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
#' @param y input time series.
#' @param periods numeric vector of seasonal periods. Each value must be a
#'   positive real number (e.g. 7 for weekly, 365.2425 for annual seasonality).
#' @param x optional matrix of user-defined regression variables (e.g. calendar
#'   regressors built using \code{rjd3toolkit}).
#' @param ndiff integer specifying the number of regular differences.
#'   Default is 2.
#' @param ar logical. If TRUE, an autoregressive component is included in the
#'   model. Default is FALSE.
#' @param outliers character vector specifying the types of outliers to detect.
#'   Possible values include \code{"AO"}, \code{"LS"} and \code{"WO"}.
#'   Default is \code{NULL} (no automatic outlier detection).
#' @param criticalValue numeric. Critical value for automatic outlier detection.
#'   Larger values imply more conservative detection. Default is 6.
#' @param precision numeric. Precision of the likelihood optimization.
#'   Default is \code{1e-12}.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param approximateHessian logical. If TRUE, compute an approximate Hessian
#'   matrix based on the optimization procedure. Default is FALSE.
#' @param nfcasts number of forecasts. Default is 0.
#' @param log logical. If TRUE, the model is estimated on the log-scale.
#'   Default is FALSE.
#' @param y_time optional vector of time indices associated with \code{y}.
#'
#' @return
#' An object of class \code{"JDFractionalAirlineEstimation"} containing:
#' \itemize{
#'   \item the original and linearized series,
#'   \item estimated regression effects and outlier components,
#'   \item model parameters and covariance matrices,
#'   \item likelihood and diagnostic information.
#' }
#'#'
#' @examples
#' # input data
#' # Linearize the series using weekly and annual periodicities
#' pre_proc_births <- fractionalAirlineEstimation(
#'   y = Births$births,
#'   periods = c(7, 365.2425),
#'   log = FALSE,
#'   y_time = Births$date
#' )
#' @export
fractionalAirlineEstimation <- function(y,
                                        periods,
                                        x = NULL,
                                        ndiff = 2,
                                        ar = FALSE,
                                        outliers = NULL,
                                        criticalValue = 6,
                                        precision = 1e-12,
                                        deps=1e-4,
                                        approximateHessian = FALSE,
                                        nfcasts = 0,
                                        log = FALSE,
                                        y_time = NULL) {

    # Input checks
    mean <- FALSE
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(criticalValue, len = 1, null.ok = FALSE)
    checkmate::assertNumeric(precision, len = 1, null.ok = FALSE)
    checkmate::assertLogical(mean, len = 1, null.ok = FALSE)


    if (is.null(outliers)) {
        joutliers <- .jnull("[Ljava/lang/String;")
    } else {
        joutliers <- .jarray(outliers, "java.lang.String")
    }
    jrslt <- .jcall(
        obj = "jdplus/highfreq/base/r/FractionalAirlineProcessor",
        returnSig = "Ljdplus/highfreq/base/core/extendedairline/ExtendedAirlineEstimation;",
        method = "estimate",
        as.numeric(y),
          log,
      rjd3toolkit::.r2jd_matrix(x),
        mean,
        .jarray(periods),
        as.integer(ndiff),
        ar,
        joutliers,
        criticalValue,
        as.integer(nfcasts),
        precision,
        deps,
        approximateHessian
    )

    external_variables <- .proc_variable_outlier_names(
        var_out_names = jrslt$getOutliers(),
        nX = jrslt$getNx()
    )
    reg_mat <- rjd3toolkit::.proc_matrix(rslt = jrslt, name = "regressors")

    if (is.null(y_time) && !is.null(x)) {
        y_time <- rownames(x)
    }

    if (!is.null(colnames(x)) && sum(duplicated(colnames(x))) == 0) {
        external_variables[seq_len(ncol(x))] <- colnames(x)
        # Outliers
        if (!is.null(y_time) && (length(external_variables) - ncol(x) > 0)) {
            outliers <- external_variables[-seq_len(ncol(x))]
            outliers_type <- substr(outliers, start = 1, stop = 2)
            outliers_date <- y_time[as.integer(substr(outliers, start = 4, stop = 50))]
            external_variables[-seq_len(ncol(x))] <- paste0(outliers_type, ".", outliers_date)
        }
    } else if (is.null(x)
               && !is.null(y_time)
               && length(external_variables) > 0) {
        outliers <- external_variables
        outliers_type <- substr(outliers, start = 1, stop = 2)
        outliers_date <- y_time[as.integer(substr(outliers, start = 4, stop = 50))]
        external_variables <- paste0(outliers_type, ".", outliers_date)
    }
    if (!is.null(reg_mat) && ncol(reg_mat) > 0) {
        colnames(reg_mat) <- external_variables
    }
    if (!is.null(reg_mat) && nrow(reg_mat) > 0 && !is.null(y_time)) {
        rownames(reg_mat) <- y_time
    }

    model <- list(
        y = rjd3toolkit::.proc_vector(jrslt, "y"),
        y_time = y_time,
        periods = periods,
        variables = external_variables,
        # "variables " names of variables and outliers
        xreg = reg_mat,
        # "xreg" matrix of regressor (external variables and outliers)
        b = rjd3toolkit::.proc_vector(jrslt, "b"),
        bcov = rjd3toolkit::.proc_matrix(jrslt, "bvar"),
        linearized = rjd3toolkit::.proc_vector(jrslt, "lin"),
        residuals = rjd3toolkit::.proc_vector(jrslt,"residuals"),
        component_wo = rjd3toolkit::.proc_vector(jrslt, "component_wo"),
        component_ao = rjd3toolkit::.proc_vector(jrslt, "component_ao"),
        component_ls = rjd3toolkit::.proc_vector(jrslt, "component_ls"),
        component_outliers = rjd3toolkit::.proc_vector(jrslt, "component_outliers"),
        component_userdef_reg_variables = rjd3toolkit::.proc_vector(jrslt, "component_userdef_reg_variables"),
        component_mean = rjd3toolkit::.proc_vector(jrslt, "component_mean"),
        log=rjd3toolkit::.proc_bool(jrslt,"log"),
        missingOrNegative = rjd3toolkit::.proc_vector(jrslt, "missing")
    )

    estimation <- list(parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
                       score = rjd3toolkit::.proc_vector(jrslt, "score"),
                       covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"))

    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

    return(structure(list(model = model,
                          estimation = estimation,
                          likelihood = likelihood),
                     class = "JDFractionalAirlineEstimation"))
}

.proc_variable_outlier_names<-function(var_out_names,nX) {
  o<-.jevalArray(var_out_names)
  nO<-length(o)
  if (nO > 0) {
    regvar_outliers<-rep(NA,nX-nO)
    for(j in 1:nX-nO) {
      regvar_outliers[j] <- paste("x-", j)}
    for (j in 1:nO) {
      regvar_outliers[nX-nO+j] <- o[[j]]$toString()}
    return(regvar_outliers)
  } else {
    return(list())
  }
}

#' Perform a raw multi-period AMB decomposition using fractional airline models
#'
#' This function performs an Arima Model Based (AMB) decomposition with one or
#' more seasonal periodicities, and returns the raw Java decomposition object
#' without any post-processing or conversion to R structures.
#'
#' It is mainly intended for internal use or for advanced users who need direct
#' access to the underlying Java object
#' (\code{LightExtendedAirlineDecomposition}).
#'
#' @param y input time series.
#' @param periods numeric vector of seasonal periods. Each value must be a
#'   positive real number (e.g. 7 for weekly, 365.2425 for annual seasonality).
#' @param ndiff integer specifying the number of regular differences.
#'   Default is 2.
#' @param ar logical. If TRUE, an autoregressive component is included in the
#'   model. Default is FALSE.
#' @param stde logical. If TRUE, compute standard deviations of the components.
#'   Default is FALSE.
#' @param nbcasts number of backcasts. Default is 0.
#' @param nfcasts number of forecasts. Default is 0.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param precision numeric. Precision of the likelihood optimization.
#'   Default is \code{1e-12}.
#'
#' @return
#' A Java object of class
#' \code{jdplus.highfreq.base.core.extendedairline.decomposition.LightExtendedAirlineDecomposition}
#' containing the raw results of the AMB decomposition.
#'
#'
#' @examples
#' # pre_proc_births$linearized is a linearized series. On Births data
#' # without linearization the decomposition fails.
#'
#' # Raw decomposition with weekly and annual periodicities
#' jdec <- multiAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   periods = c(7, 365.2425)
#' )
#' @export
multiAirlineDecomposition_raw<-function(y, periods, ndiff=2, ar=FALSE, stde=FALSE, nbcasts=0, nfcasts=0, precision=1e-12, deps=1e-4) {
    checkmate::assertNumeric(y, null.ok = FALSE)

    jrslt<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                  "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                  "decompose", as.numeric(y),
                  .jarray(periods), as.integer(ndiff), ar, stde, as.integer(nbcasts), as.integer(nfcasts), as.numeric(precision), as.numeric((deps)))

    return(jrslt)
}
#' Extract the state space form (SSF) representation from a multi-airline
#' sdecomposition
#'
#' This function extracts the state space form (SSF) representation associated
#' with a fractional airline AMB decomposition. The SSF object is obtained from
#' the Java backend and converted to an R-friendly representation using
#' \code{rjd3toolkit}.
#'
#' It is mainly intended for diagnostic purposes or for advanced users who need
#' access to the underlying state space model corresponding to the estimated
#' Ucarima representation.
#'
#' @param jdecomp Java object returned by
#'   \code{multiAirlineDecomposition_raw()} or an equivalent fractional airline
#'   decomposition.
#'
#' @return
#' An R object representing the state space form of the decomposition, derived
#' from a Java \code{SsfUcarimaEstimation} object.
#'
#' @export
#'
#' @examples
#' # Using the linearized series; decomposition fails on raw (non-linearized)
#' # birth data.
#'
#' # Raw multi-period decomposition
#' jdec <- multiAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   periods = c(7, 365.2425)
#' )
#'
#' # Extract the state space form
#' ssf <- multiAirlineDecomposition_ssf(jdec)
multiAirlineDecomposition_ssf<-function(jdecomp) {
    jssf<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                 "Ljdplus/highfreq/base/core/ssf/extractors/SsfUcarimaEstimation;", "ssfDetails", jdecomp)
    return(rjd3toolkit::.jd3_object(jssf, result=TRUE))
}

#' Perform a raw AMB decomposition using a fractional airline model
#'
#' This function performs an Arima Model Based (AMB) decomposition based on a
#' fractional airline model for a single seasonal periodicity and returns the
#' raw Java decomposition object without any conversion to R structures.
#'
#' It is mainly intended for internal use or for advanced users who need direct
#' access to the underlying Java
#' \code{LightExtendedAirlineDecomposition} object.
#'
#' @param y input time series.
#' @param period numeric value specifying the seasonal period. Must be a
#'   positive real number (e.g. 7 for weekly, 365.2425 for annual seasonality).
#' @param sn logical. If TRUE, perform a signal–noise decomposition (two
#'   components only). The signal corresponds to the seasonally adjusted series
#'   and the noise to the seasonal component. Default is FALSE.
#' @param stde logical. If TRUE, compute standard deviations of the components.
#'   Default is FALSE.
#' @param nbcasts number of backcasts. Default is 0.
#' @param nfcasts number of forecasts. Default is 0.
#' @param deps step in the computation of the numerical derivatives, used in the optimisation routine. Default:1e-4
#' @param precision numeric. Precision of the likelihood optimization.
#'
#' @return
#' A Java object of class
#' \code{jdplus.highfreq.base.core.extendedairline.decomposition.LightExtendedAirlineDecomposition}
#' containing the raw results of the AMB decomposition.
#'
#' @export
#'
#' @examples
#'
#' # Using the linearized series; decomposition fails on raw (non-linearized)
#' # birth data.
#'
#' # Raw decomposition with weekly periodicity
#' jdec <- fractionalAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   period = 7
#' )
fractionalAirlineDecomposition_raw<-function(y, period, sn=FALSE, stde=FALSE, nbcasts=0, nfcasts=0, precision=1e-12, deps=1e-4) {
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(period, len = 1, null.ok = FALSE)
    checkmate::assertLogical(sn, len = 1, null.ok = FALSE)
    jrslt<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                  "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                  "decompose", as.numeric(y),
                  period, sn, stde, as.integer(nbcasts), as.integer(nfcasts), as.numeric(precision), as.numeric(deps))
    return(jrslt)
}

#' Extract the state space form (SSF) representation from a fractional airline decomposition
#'
#' This function extracts the state space form (SSF) representation associated
#' with a fractional airline AMB decomposition for a single seasonal periodicity.
#' The SSF object is obtained from the Java backend and converted to an
#' R-friendly representation using \code{rjd3toolkit}.
#'
#' It is mainly intended for diagnostic purposes or for advanced users who need
#' access to the underlying state space model corresponding to the estimated
#' Ucarima representation.
#'
#' @param jdecomp Java object returned by
#'   \code{fractionalAirlineDecomposition_raw()} or an equivalent fractional
#'   airline decomposition.
#'
#' @return
#' An R object representing the state space form of the decomposition, derived
#' from a Java \code{SsfUcarimaEstimation} object.
#'
#' @export
#'
#' @examples
#' # Raw fractional airline decomposition
#' jdec <- fractionalAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   period = 7
#' )
#'
#' # Extract the state space form
#' ssf <- fractionalAirlineDecomposition_ssf(jdec)
fractionalAirlineDecomposition_ssf<-function(jdecomp) {
    jssf<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor", "Ljdplus/highfreq/base/core/ssf/extractors/SsfUcarimaEstimation;", "ssfDetails", jdecomp)
    return(rjd3toolkit::.jd3_object(jssf, result=TRUE))
}


#' Convert a raw multi-period AMB decomposition to an R-friendly object
#'
#' This function takes a Java object returned by
#' \code{multiAirlineDecomposition_raw()} and converts it into a structured R
#' object of class \code{JDFractionalAirlineDecomposition}. The result includes
#' the estimated Ucarima model, the decomposition components, standard errors
#' (optional), parameter estimates, and likelihood diagnostics.
#'
#' @param jrslt Java object returned by \code{multiAirlineDecomposition_raw()}.
#' @param stde logical. If TRUE, include standard deviations of the components
#'   in the returned decomposition. Default is FALSE.
#' @param log logical. If TRUE, indicates that the decomposition was performed
#'   on a log-transformed series. Default is FALSE.
#' @param y_time optional vector of time indices associated with the series
#'   \code{y}. Default is NULL.
#' @param periods numeric vector of seasonal periods corresponding to the
#'   decomposition.
#'
#' @return
#' An object of class \code{JDFractionalAirlineDecomposition} containing:
#' \itemize{
#'   \item \code{ucarima}: the Ucarima model with its components,
#'   \item \code{decomposition}: list of original series, seasonally adjusted
#'     series, and component time series (with optional standard deviations),
#'   \item \code{estimation}: estimated parameters, covariance matrix, and
#'     score,
#'   \item \code{likelihood}: likelihood diagnostics.
#' }
#'
#'
#' @examples
#'
#' # Using the linearized series; decomposition fails on raw (non-linearized)
#' # birth data.
#'
#' # Raw multi-period decomposition
#' jdec <- multiAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   periods = c(7, 365.2425)
#' )
#'
#' # Convert to R object with decomposition
#' amb_r <- jd2r_multiAirlineDecomposition(jdec,
#'                                        stde = TRUE,
#'                                        periods = c(7, 365.2425),
#'                                        log = FALSE,
#'                                        y_time = pre_proc_births$model$y_time)
#'
#' # Access the seasonally adjusted series
#' sa <- amb_r$decomposition$sa
#'
#' @export
jd2r_multiAirlineDecomposition <- function(jrslt, stde = FALSE, periods,
                                           log = FALSE, y_time = NULL) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- .arima_extract(jrslt, "ucarima.model")
    cmps <- lapply(1:ncmps, function(cmp) {
        return(.ucm_extract(jrslt, cmp))
    })
    ucarima <- rjd3toolkit::ucarima_model(model, cmps)
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    estimation <- list(
        parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
        score = rjd3toolkit::.proc_vector(jrslt, "score"),
        covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
        periods = periods,
        log = log)
    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

    ncmps <- rjd3toolkit::.proc_int(jrslt, "ncmps")
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    sa <- rjd3toolkit::.proc_vector(jrslt, "sa")
    tsi_component <- lapply(X = 1:ncmps, FUN = function(j) {
        return(rjd3toolkit::.proc_vector(jrslt, paste0("cmp(", j, ")")))
    })
    if (ncmps == length(periods)+2){
        names(tsi_component) <- c("t", paste0("s_", periods) , "i")
    }else{
        names(tsi_component) <- c("t", paste0("s_", periods) )
    }
    decomposition <- c(
        list(y = yc, y_time = y_time, sa = sa),
        tsi_component)

    if (stde) {
        tsi_stde_component <- lapply(X = 1:ncmps, FUN = function(j) {
            return(rjd3toolkit::.proc_vector(jrslt, paste0("cmp_stde(", j, ")")))
        })
        names(tsi_stde_component) <- c("t.stde", paste0("stde_", periods) , "i.stde")

        decomposition <- c(decomposition, tsi_stde_component)
    }

    return(structure(list(ucarima = ucarima,
                          decomposition = decomposition,
                          estimation = estimation,
                          likelihood = likelihood),
                     class = "JDFractionalAirlineDecomposition"))
}


#' Convert a raw fractional airline decomposition to an R-friendly object
#'
#' This function takes a Java object returned by
#' \code{fractionalAirlineDecomposition_raw()} and converts it into a structured
#' R object of class \code{JDFractionalAirlineDecomposition}. The result includes
#' the estimated Ucarima model, the decomposition components, standard errors
#' (optional), parameter estimates, and likelihood diagnostics.
#'
#' @param jrslt Java object returned by \code{fractionalAirlineDecomposition_raw()}.
#' @param sn logical. If TRUE, perform a signal–noise decomposition (2 components
#'   only: seasonally adjusted series and seasonal component). Default is FALSE.
#' @param stde logical. If TRUE, include standard deviations of the components
#'   in the returned decomposition. Default is FALSE.
#' @param log logical. If TRUE, indicates that the decomposition was performed
#'   on a log-transformed series. Default is FALSE.
#' @param y_time optional vector of time indices associated with the series
#'   \code{y}. Default is NULL.
#' @param period numeric. Seasonal period corresponding to the decomposition.
#'
#' @return
#' An object of class \code{JDFractionalAirlineDecomposition} containing:
#' \itemize{
#'   \item \code{ucarima}: the Ucarima model with its components,
#'   \item \code{decomposition}: list of original series, seasonally adjusted
#'     series, and components (trend, irregular, seasonal), with optional
#'     standard deviations,
#'   \item \code{estimation}: estimated parameters, covariance matrix, and score,
#'   \item \code{likelihood}: likelihood diagnostics.
#' }
#'
#' @export
#'
#' @examples
#'
#' # Using the linearized series; decomposition fails on raw (non-linearized)
#' # birth data.
#'
#' # Raw fractional airline decomposition
#' jdec <- fractionalAirlineDecomposition_raw(
#'   y = pre_proc_births$model$linearized,
#'   period = 7
#' )
#'
#' # Convert to R object with decomposition
#' amb_r <- jd2r_fractionalAirlineDecomposition(
#'   jdec,
#'   sn = FALSE,
#'   stde = TRUE,
#'   period = 7,
#'   log = FALSE,
#'   y_time = pre_proc_births$model$y_time
#' )
#'
#' # Access the seasonally adjusted series
#' sa <- amb_r$decomposition$sa
jd2r_fractionalAirlineDecomposition <- function(jrslt,
                                                sn = FALSE,
                                                stde = FALSE,
                                                period,
                                                log = FALSE,
                                                y_time = NULL) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- .arima_extract(jrslt, "ucarima.model")
    cmps <- lapply(
        X = 1:ncmps,
        FUN = function(cmp) .ucm_extract(jrslt, cmp)
    )
    ucarima <- rjd3toolkit::ucarima_model(model, cmps)
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    sa <- rjd3toolkit::.proc_vector(jrslt, "sa")
    s <- rjd3toolkit::.proc_vector(jrslt, "s")

    decomposition <- list(y = yc, y_time = y_time, sa = sa, s = s)

    if (!sn) {
        tc <- rjd3toolkit::.proc_vector(jrslt, "t")
        ic <- rjd3toolkit::.proc_vector(jrslt, "i")
        decomposition <- c(decomposition, list(t = tc, i = ic))
    }

    if (stde) {
        s.stde <- rjd3toolkit::.proc_vector(jrslt, "s_stde")
        decomposition <- c(decomposition, list(s.stde = s.stde))

        if (!sn) {
            t.stde <- rjd3toolkit::.proc_vector(jrslt, "t_stde")
            i.stde <- rjd3toolkit::.proc_vector(jrslt, "i_stde")
            decomposition <- c(decomposition,
                               list(t.stde = t.stde, i.stde = i.stde))
        }
    }

    estimation <- list(
        parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
        score = rjd3toolkit::.proc_vector(jrslt, "score"),
        covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
        periods = period,
        log = log)

    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

    return(structure(list(ucarima = ucarima,
                          decomposition = decomposition,
                          estimation = estimation,
                          likelihood = likelihood),
                     class = "JDFractionalAirlineDecomposition"))
}

#' Full Example 1: Linearization, Multi-Period Decomposition, and Plot
#'
#' This example demonstrates a complete workflow using R wrappers of JD+ Java
#' - Fractional Airline Estimation
#' - Linearization of the series
#' - Multi-period decomposition
#' - Plotting with JD+ style
#'
#' @examples
#'
#' options(java.parameters = "-Xmx8g") # High RAM usage expected
#'
#' library("rjd3toolkit")
#' library("rjd3highfreq")
#'
#'
#' # 1. Fractional Airline Estimation (linearization)
#'
#' # We use pre-computed data
#' # pre_proc_births <- fractionalAirlineEstimation(
#' #  y             = Births$births,
#' #  periods       = c(7, 365.2425),
#' #  log           = FALSE,
#' #  outliers      = c("ao", "wo", "ls"),
#' #  criticalValue = 6,
#' #  y_time        = Births$date
#' # )
#'
#'
#' linearized_data <- list(
#'   y = pre_proc_births$model$linearized,
#'   date = pre_proc_births$model$y_time
#' )
#'
#' # 2. Multi-period decomposition (raw Java object)
#' jdec <- multiAirlineDecomposition_raw(
#'   y = linearized_data$y,
#'   periods = c(7, 365.2425),
#'   stde = TRUE
#' )
#'
#' # 3. Convert to R decomposition object
#' amb_r <- jd2r_multiAirlineDecomposition(
#'   jrslt = jdec,
#'   stde = TRUE,
#'   periods = c(7, 365.2425),
#'   y_time = linearized_data$date
#' )
#'
#' # 4. Plot raw and linearized series
#' plot(pre_proc_births)
#'
#' # 5. Plot decomposition (y, seasonally adjusted, trend)
#' plot(amb_r, type_chart = "y-sa-trend")
#'
#' # Example: Fractional Airline Estimation with Calendar Regressors and
#' # Multi-Period Decomposition
#' #
#' # NOTE: Sequential decomposition is used (day of the week -> day of the year)
#' # that could be more stable than the multiAirlineDecomposition
#'
#' # This example demonstrates a complete workflow using the JD+ R functions:
#' # 1. Load daily data.
#' # 2. Define a French calendar with holidays.
#' # 3. Create calendar regressors.
#' # 4. Estimate a fractional airline model including outliers and log transformation.
#' # 5. Plot raw and linearized series.
#' # 6. Perform single-period decomposition (weekly and yearly patterns).
#' # 7. Perform multi-period decomposition (weekly + yearly).
#' # 8. Plot decompositions over the full series or selected intervals.
#'
#' @examples
#' options(java.parameters = "-Xmx8g") # some computations may require many RAM
#' library("rjd3toolkit")
#' library("rjd3highfreq")
#'
#'
#' # Define French calendar
#' frenchCalendar <- rjd3toolkit::national_calendar(days = list(
#'   fixed_day(7, 14),
#'   fixed_day(5, 8, validity = list(start = "1982-05-08")),
#'   special_day('NEWYEAR'),
#'   special_day('MAYDAY'),
#'   special_day('EASTERMONDAY'),
#'   special_day('ASCENSION'),
#'   special_day('WHITMONDAY'),
#'   special_day('ASSUMPTION'),
#'   special_day('ALLSAINTSDAY'),
#'   special_day('ARMISTICE'),
#'   special_day('CHRISTMAS'))
#' )
#'
#' # Calendar regressors matrix
#' cal_reg <- holidays(
#'   calendar = frenchCalendar,
#'   start = "1968-01-01",
#'   length = nrow(Births),
#'   type = "All", nonworking = 7L
#' )
#' colnames(cal_reg) <- c("14th_july", "8th_may", "1st_jan", "1st_may",
#'                        "east_mon", "asc", "pen_mon",
#'                        "15th_aug", "1st_nov", "11th_nov", "Xmas")
#'
#' # 4. Estimate fractional airline model with weekly frequency
#' # We use pre-computed data
#' # pre_proc_births <- fractionalAirlineEstimation(
#' #  y             = Births$births,
#' #  periods       = c(7, 365.2425),
#' #  log           = FALSE,
#' #  outliers      = c("ao", "wo", "ls"),
#' #  criticalValue = 6,
#' #  x             = cal_reg,
#' #  y_time        = Births$date
#' # )
#'
#' # 5. Plot raw and linearized series
#' plot(pre_proc_births, main = "French births")
#' plot(pre_proc_births,
#'      from = as.Date("2019-01-01"),
#'      to = as.Date("2019-12-31"),
#'      main = "French births in 2019")
#'
#' # 6. Decomposition with weekly pattern
#' amb.dow <- fractionalAirlineDecomposition(
#'   y = pre_proc_births$model$linearized,
#'   period = 7,
#'   log = FALSE,
#'   y_time = pre_proc_births$model$y_time
#' )
#'
#' # Extract yearly pattern from DOW-adjusted linearised data
#' amb.doy <- fractionalAirlineDecomposition(
#'   y = amb.dow$decomposition$sa,
#'   period = 365.2425,
#'   log = FALSE,
#'   y_time = pre_proc_births$model$y_time
#' )
#'
#' # 7. Plot decompositions
#' plot(amb.dow, main = "Weekly pattern")
#' plot(amb.dow,
#'      main = "Weekly pattern - January 2019",
#'      from = as.Date("2019-01-01"),
#'      to = as.Date("2019-01-31"))
#'
#' plot(amb.doy, main = "Yearly pattern")
#' plot(amb.doy,
#'      main = "Weekly pattern - 2019-2021",
#'      from = as.Date("2019-01-01"),
#'      to = as.Date("2021-12-31"))
