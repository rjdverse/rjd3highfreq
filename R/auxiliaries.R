
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
#' @keywords internal

.arima_extract <- function(jrslt, path) {
  str <- rjd3toolkit::.proc_str(jrslt, paste0(path, ".name"))
  ar <- rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ar"))
  delta <- rjd3toolkit::.proc_vector(jrslt, paste0(path, ".delta"))
  ma <- rjd3toolkit::.proc_vector(jrslt, paste0(path, ".ma"))
  var <- rjd3toolkit::.proc_numeric(jrslt, paste0(path, ".var"))
  return(rjd3toolkit::arima_model(str, ar, delta, ma, var))
}



#' Extract a specific UCM component from a Java UcarimaModel object
#'
#' This internal function retrieves a single component from a UCM
#' (Unobserved Components Model) result object returned by the Java backend.
#' The component can be specified either by name or by its integer code.
#'
#' @param jrslt Java UcarimaModel object.
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
#' # Assume `jucm` is a UcarimaModel Java object
#'
#' # Extract the trend component
#' trend <- .ucm_extract(jucm, "Trend")
#' }
#' @keywords internal

.ucm_extract <- function(jrslt, cmp) {
  path <- paste0("ucarima.component(", cmp, ")")
  return(.arima_extract(jrslt, path))
}

#' Convert an R numeric vector to a Java DoubleSeq object
#'
#' This internal helper function converts an R object to a Java
#' \code{DoubleSeq} object from the JDemetra+ toolkit.
#'
#' The input is first coerced to numeric using \code{as.numeric()} and is then
#' passed to the static Java factory method \code{DoubleSeq.of()}.
#'
#' @param x An object that can be coerced to a numeric vector.
#'
#' @return A Java object of class
#' \code{jdplus.toolkit.base.api.data.DoubleSeq}.
#'
#' @details
#' This function is mainly intended for internal use when passing numeric R
#' vectors to Java methods that expect a \code{DoubleSeq}.
#'
#' @examples
#' \dontrun{
#' x <- c(1.2, 3.4, 5.6)
#' jseq <- .r2jd_doubleseq(x)
#' }
#'
#' @keywords internal

.r2jd_doubleseq <- function(x) {
  return(rJava::.jcall("jdplus/toolkit/base/api/data/DoubleSeq",
                "Ljdplus/toolkit/base/api/data/DoubleSeq;",
                "of", as.numeric(x)))
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
#' @param series_time optional vector of time indices associated with the
#'   \code{series}. Default is NULL.
#' @param period numeric vector of seasonal periods corresponding to the
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
#' @keywords internal

.jd2r_multi_airline_decomposition <- function(jrslt,
                                              stde = FALSE,
                                              period,
                                              log = FALSE,
                                              series_time = NULL) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- .arima_extract(jrslt, "ucarima.model")

    cmps <- lapply(1:ncmps, function(cmp) {
        return(.ucm_extract(jrslt, cmp))
    })

    ucarima <- rjd3toolkit::ucarima_model(model, cmps)

    estimation <- list(
        parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
        score = rjd3toolkit::.proc_vector(jrslt, "score"),
        covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
        period = period,
        log = log
    )

    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

    ncmps <- rjd3toolkit::.proc_int(jrslt, "ncmps")
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    sa <- rjd3toolkit::.proc_vector(jrslt, "sa")

    components <- jrslt$getComponents()
    size <- rJava::.jcall(components, returnSig = "I", method = "size")

    tsi_component <- sapply(
        X = (1:size) - 1,
        FUN = function(j) {
            comp <- rJava::.jcall(components,
                                  returnSig = "Ljava/lang/Object;",
                                  method = "get",
                                  as.integer(j))
            z <- comp$getData()$toArray()
            return(z)
        }
    )


    colnames(tsi_component) <- sapply(
        X = (1:size) - 1,
        FUN = function(j) {
            comp <- rJava::.jcall(components,
                                  returnSig = "Ljava/lang/Object;",
                                  method = "get",
                                  as.integer(j))
            return(comp$getName() |> tolower())
        }
    )


    if (is.null(dim(tsi_component))) {
        # essentially if tsi_component is only a vector
        tsi_component <- tsi_component |> t() |> as.data.frame(check.names = FALSE)
    } else {
        # if tsi_component is a data.frame
        tsi_component <- tsi_component |> as.data.frame(check.names = FALSE)
    }


    decomposition <- data.frame(
        series = yc,
        sa = sa,
        tsi_component = tsi_component,
        check.names = FALSE
    )

    if (stde) {
        standard_deviations <- sapply(
            X = (1:size) - 1,
            FUN = function(j) {
                comp <- rJava::.jcall(components,
                                      returnSig = "Ljava/lang/Object;",
                                      method = "get",
                                      as.integer(j))
                z <- comp$getStde()$toArray()
                return(z)
            }
        )

        colnames(standard_deviations) <- sapply(
            X = (1:size) - 1,
            FUN = function(j) {
                comp <- rJava::.jcall(components,
                                      returnSig = "Ljava/lang/Object;",
                                      method = "get",
                                      as.integer(j))
                stde_names <- paste0("stde_", tolower(comp$getName()))
                return(stde_names)
            }
        )

        if (is.null(dim(standard_deviations))) {
            # essentially if standard_deviations is only a vector
            standard_deviations <- standard_deviations |> t() |> as.data.frame(check.names = FALSE)
        } else {
            # if standard_deviations is a data.frame
            standard_deviations <- standard_deviations |> as.data.frame(check.names = FALSE)
        }

        decomposition <- cbind(decomposition, standard_deviations)
    }


    parameters <- list(
        log = log,
        period = period,
        stde = stde,
        series_time = series_time
    )

    return(structure(
        list(
            decomposition = decomposition,
            parameters = parameters,
            ucarima = ucarima,
            estimation = estimation,
            likelihood = likelihood
        ),
        class = "hf_decomposition"
    ))
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
#' @param series_time optional vector of time indices associated with the
#'   \code{series}. Default is NULL.
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
#' @keywords internal

.jd2r_fractional_airline_decomposition <- function(jrslt,
                                                   sn = FALSE,
                                                   stde = FALSE,
                                                   period,
                                                   log = FALSE,
                                                   series_time = NULL) {
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

  decomposition <- data.frame(
    series = yc,
    sa = sa,
    s = s
  )

  if (!sn) {
    decomposition$t <- rjd3toolkit::.proc_vector(jrslt, "t")
    decomposition$i <- rjd3toolkit::.proc_vector(jrslt, "i")
    decomposition$series_time <- series_time
  } else {
    decomposition$series_time <- series_time
  }

  if (stde) {
    s_stde <- rjd3toolkit::.proc_vector(jrslt, "s_stde")
    decomposition <- list(decomposition, list(s_stde = s_stde))

    if (!sn) {
      t_stde <- rjd3toolkit::.proc_vector(jrslt, "t_stde")
      i_stde <- rjd3toolkit::.proc_vector(jrslt, "i_stde")
      decomposition <- c(decomposition,
                         list(t_stde = t_stde,
                              i_stde = i_stde))
    }
  }

  estimation <- list(parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
                     score = rjd3toolkit::.proc_vector(jrslt, "score"),
                     covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
                     periods = period,
                     log = log)

  likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

  decomposition <- decomposition[,c("series", "sa", "t", "s", "i")]

  parameters <- list(
      log = log,
      period = period,
      sn = sn,
      stde = stde,
      series_time = series_time
  )

  return(structure(list(decomposition = decomposition,
                        parameters = parameters,
                        ucarima = ucarima,
                        estimation = estimation,
                        likelihood = likelihood),
                   class = "hf_decomposition"))
}


.proc_variable_outlier_names <- function(var_out_names, n_x) {
  o <- rJava::.jevalArray(var_out_names)
  n_o <- length(o)
  if (n_o > 0) {
    regvar_outliers <- rep(NA, n_x - n_o)
    for (j in 1:n_x - n_o) {
      regvar_outliers[j] <- paste("x-", j)
    }
    for (j in 1:n_o) {
      regvar_outliers[n_x - n_o + j] <- o[[j]]$toString()
    }
    return(regvar_outliers)
  } else {
    return(list())
  }
}
