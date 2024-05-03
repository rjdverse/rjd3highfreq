#' @include utils.R
NULL

#' Title
#'
#' @param jrslt
#' @param cmp
#'
#' @return
#' @export
#'
#' @examples
.ucm_extract<-function(jrslt, cmp) {
    path<-paste0("ucarima.component(", cmp,")")
    return (.arima_extract(jrslt, path))
}

#' Title
#'
#' @param jrslt
#' @param path
#'
#' @return
#' @export
#'
#' @examples
.arima_extract<-function(jrslt, path) {
    str<-rjd3toolkit::.proc_str(jrslt, paste0(path, ".name"))
    ar<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ar"))
    delta<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".delta"))
    ma<-rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ma"))
    var<-rjd3toolkit::.proc_numeric(jrslt, paste0(path, ".var"))
    return (rjd3toolkit::arima_model(str, ar,delta,ma,var))
}



#' Perform an Arima Model Based (AMB) decomposition
#'
#' @param y input time series.
#' @param period period of the seasonal component, any positive real number.
#' @param adjust Boolean: TRUE: actual fractional airline model is to be used, FALSE: the period is rounded to the nearest integer.
#' @param sn decomposition into signal and noise (2 components only). The signal is the seasonally adjusted series and the noise the seasonal component.
#' @param stde Boolean: TRUE: compute standard deviations of the components. In some cases (memory limits), it is currently not possible to compute them
#' @param nbcasts number of backcasts.
#' @param nfcasts number of forecasts.
#' @param log
#' @param y_time vector of times at which `y` is indexed
#'
#' @return
#' @export
#'
#' @examples
#'
fractionalAirlineDecomposition <- function(y,
                                           period,
                                           sn = FALSE,
                                           stde = FALSE,
                                           nbcasts = 0,
                                           nfcasts = 0,
                                           log = FALSE,
                                           y_time = NULL) {
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(period, len = 1, null.ok = FALSE)
    checkmate::assertLogical(sn, len = 1, null.ok = FALSE)
    jrslt <- .jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                    "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                    "decompose", as.numeric(y), period, sn, stde, as.integer(nbcasts),
                    as.integer(nfcasts))
    return(jd2r_fractionalAirlineDecomposition(jrslt, sn, stde, period, log, y_time))
}


#' Perform an Arima Model Based (AMB) decomposition on several periodcities at once
#'
#' @param y input time series.
#' @param periods vector of periods values of the seasonal component, any positive real numbers.
#' @param adjust Boolean: TRUE: actual fractional airline model is to be used, FALSE: the period is rounded to the nearest integer.
#' @param sn decomposition into signal and noise (2 components only). The signal is the seasonally adjusted series and the noise the seasonal component.
#' @param stde Boolean: TRUE: compute standard deviations of the components. In some cases (memory limits), it is currently not possible to compute them
#' @param nbcasts number of backcasts.
#' @param nfcasts number of forecasts.
#' @param log
#' @param y_time vector of times at which `y` is indexed
#'
#' @return
#' @export
#'
#' @examples
#'
multiAirlineDecomposition <- function(y, periods, ndiff = 2, ar = FALSE, stde = FALSE,
                                      nbcasts = 0, nfcasts = 0, log = FALSE, y_time = NULL) {
    if (length(periods) == 1) {
        return(fractionalAirlineDecomposition(y, periods, stde = stde,
                                              nbcasts = nbcasts, nfcasts = nfcasts,
                                              log = log, y_time = y_time))
    }
    checkmate::assertNumeric(y, null.ok = FALSE)
    jrslt <- .jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                    "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                    "decompose", as.numeric(y), .jarray(periods), as.integer(ndiff),
                    ar, stde, as.integer(nbcasts), as.integer(nfcasts))
    if (length(periods) == 1) {
        return(jd2r_fractionalAirlineDecomposition(jrslt, sn = FALSE, stde, periods,
                                                   log = log, y_time = y_time))
    } else {
        return(jd2r_multiAirlineDecomposition(jrslt, stde, periods,
                                              log = log, y_time = y_time))
    }
}


#' Linearize the series with a fractional airline model
#'
#' @param y input time series.
#' @param periods vector of periods values of the seasonal component, any positive real numbers.
#' @param x matrix of user-defined regression variables (see rjd3toolkit for building calendar regressors).
#' @param outliers type of outliers sub vector of c("AO","LS","WO")
#' @param criticalValue Critical value for automatic outlier detection
#' @param precision Precision of the likelihood
#' @param approximateHessian Compute approximate hessian (based on the optimizing procedure)
#' @param nfcasts Number of forecasts
#' @param log a logical
#' @param y_time vector of times at which `y` is indexed
#'
#' @return
#' @export
#'
#' @examples
#'
fractionalAirlineEstimation <- function(y,
                                        periods,
                                        x = NULL,
                                        ndiff = 2,
                                        ar = FALSE,
                                        outliers = NULL,
                                        criticalValue = 6,
                                        precision = 1e-12,
                                        approximateHessian = FALSE,
                                        nfcasts = 0,
                                        log = FALSE,
                                        y_time = NULL) {

    # Input checks
    mean = FALSE
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
        rjd3toolkit::.r2jd_matrix(x),
        mean,
        .jarray(periods),
        as.integer(ndiff),
        ar,
        joutliers,
        criticalValue,
        precision,
        approximateHessian,
        as.integer(nfcasts),
        log
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

  if(nO>0){
    regvar_outliers<-rep(NA,nX-nO)
    for(j in 1:nX-nO) {
      regvar_outliers[j]=paste("x-", j)}
    for (j in 1:nO) {
      regvar_outliers[nX-nO+j]<-o[[j]]$toString()}
    return(regvar_outliers)
  }else{
    return (list())
  }
}

#' Title
#'
#' @param stde
#' @param y
#' @param periods
#' @param ndiff
#' @param stde
#' @param nbcasts
#' @param nfcasts
#'
#' @return
#' @export
#'
#' @examples
#'
multiAirlineDecomposition_raw<-function(y, periods, ndiff=2, ar=FALSE, stde=FALSE, nbcasts=0, nfcasts=0) {
    checkmate::assertNumeric(y, null.ok = FALSE)

    jrslt<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                  "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                  "decompose", as.numeric(y),
                  .jarray(periods), as.integer(ndiff), ar, stde, as.integer(nbcasts), as.integer(nfcasts))

    return (jrslt)
}

#' Title
#'
#' @param jdecomp
#'
#' @return
#' @export
#'
#' @examples
multiAirlineDecomposition_ssf<-function(jdecomp) {
    jssf<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                 "Ljdplus/highfreq/base/core/ssf/extractors/SsfUcarimaEstimation;", "ssfDetails", jdecomp)
    return (rjd3toolkit::.jd3_object(jssf, result=TRUE))
}

#' Title
#'
#' @param y
#' @param period
#' @param sn
#' @param stde
#' @param nbcasts
#' @param nfcasts
#'
#' @return
#' @export
#'
#' @examples
#'
fractionalAirlineDecomposition_raw<-function(y, period, sn=FALSE, stde=FALSE, nbcasts=0, nfcasts=0) {
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(period, len = 1, null.ok = FALSE)
    checkmate::assertLogical(sn, len = 1, null.ok = FALSE)
    jrslt<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor",
                  "Ljdplus/highfreq/base/core/extendedairline/decomposition/LightExtendedAirlineDecomposition;",
                  "decompose", as.numeric(y),
                  period, sn, stde, as.integer(nbcasts), as.integer(nfcasts))
    return (jrslt)
}

#' Title
#'
#' @param jdecomp
#'
#' @return
#' @export
#'
#' @examples
fractionalAirlineDecomposition_ssf<-function(jdecomp) {
    jssf<-.jcall("jdplus/highfreq/base/r/FractionalAirlineProcessor", "Ljdplus/highfreq/base/core/ssf/extractors/SsfUcarimaEstimation;", "ssfDetails", jdecomp)
    return (rjd3toolkit::.jd3_object(jssf, result=TRUE))
}


#' Title
#'
#' @param jrslt
#' @param stde
#' @param log
#' @param y_time vector of times at which the time series is indexed
#'
#' @return
#' @export
#'
#' @examples
#'
jd2r_multiAirlineDecomposition <- function(jrslt, stde = FALSE, periods,
                                           log = FALSE, y_time = NULL) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- .arima_extract(jrslt, "ucarima_model")
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
    names(tsi_component) <- c("t", paste0("s_", periods) , "i")

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


#' Title
#'
#' @param jrslt
#' @param sn
#' @param stde
#' @param log
#' @param y_time vector of times at which the time series is indexed
#'
#' @return
#' @export
#'
#' @examples
#'
jd2r_fractionalAirlineDecomposition <- function(jrslt,
                                                sn = FALSE,
                                                stde = FALSE,
                                                period,
                                                log = FALSE,
                                                y_time = NULL) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- .arima_extract(jrslt, "ucarima_model")
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
