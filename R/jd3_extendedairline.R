#' @include utils.R
NULL

#' Create a specification for the Extended Airline model
#'
#' This internal function constructs a Java ExtendedAirlineSpec object. 
#' The Extended Airline model is an extension of the classic seasonal ARIMA
#' model that can handle multiple simultaneous periodicities.
#'
#' @param periodicities Numeric vector of periodicities present in the data.
#'   For example, \code{c(7, 365.25)} indicates weekly and annual seasonality.
#' @param differencing Differencing order to apply. The default value
#'   \code{-1} activates automatic computation based on the number of
#'   periodicities: if \code{ar=FALSE}, the order will be 
#'   \code{length(periodicities) + 1}, otherwise it will equal 
#'   \code{length(periodicities)}.
#'   Positive values manually specify the differencing order.
#' @param ar Logical. If \code{TRUE}, uses a regular stationary autoregressive 
#'   (AR) polynomial instead of a moving average (MA) polynomial.
#'   Default: \code{FALSE}. This choice affects the automatic differencing order.
#' @param toint Logical. If \code{TRUE}, rounds periodicity values
#'   to integers before processing. Default: \code{FALSE}.
#'
#' @return A Java object of class \code{ExtendedAirlineSpec}.
#' @export
#'
#' @examples
#' # Specification for daily data with weekly and annual seasonality
#' spec1 <- .extended_airline_spec(c(7, 365.25))
#'
#' # Specification with manual differencing order
#' spec2 <- .extended_airline_spec(c(7, 365.25), differencing = 2)
#'
#' # Using AR instead of MA, with rounding of periodicities
#' spec3 <- .extended_airline_spec(c(7, 365.25), ar = TRUE, toint = TRUE)
.extended_airline_spec<-function(periodicities, differencing=-1, ar=FALSE, toint=FALSE){

    if (differencing == -1){
        differencing <- length(periodicities)
        if (! ar) differencing<-differencing + 1

    }

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/api/ExtendedAirlineSpec;", "spec",
                 .jarray(as.numeric(periodicities)), as.integer(differencing), as.logical(ar), as.logical(toint))
    return(jrslt)
}

#' Create a RegARIMA model based on an Extended Airline specification
#'
#' This internal function constructs a Java RegArimaModel object by fitting an
#' Extended Airline model.
#' @param y Numeric vector containing the time series to be modeled.
#' @param jspec A Java ExtendedAirlineSpec object, for instance created using 
#'   \code{\link{.extended_airline_spec}}. 
#' @param mean Logical. If \code{TRUE}, includes a mean correction term in the model.
#'   Default: \code{FALSE}.
#' @param X Optional matrix of regression variables.
#'   Default: \code{NULL} (no regressors).
#' @return A Java object of class \code{RegArimaModel} from the JDemetra+ toolkit.
#' @export
#'
#' @examples
#' jspec<-.extended_airline_spec(c(12))
#' .extended_airline_regarima(rjd3toolkit::ABS$X0.2.09.10.M, jspec)
.extended_airline_regarima<-function(y, jspec, mean=FALSE, X=NULL){

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/toolkit/base/core/regarima/RegArimaModel;", "regarima",
                    as.numeric(y), as.logical(mean), rjd3toolkit::.r2jd_matrix(X), jspec)
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
#' @param exactHessian Logical. If \code{TRUE}, computes the exact Hessian matrix
#'   at the optimum for calculating standard errors. If \code{FALSE} (default),
#'   uses a numerical approximation.
#' @return A list object containing detailed estimation results.
#' @export
#' 
#' @examples
#' \dontrun{
#' .extended_airline_estimation(jregarima, jspec, eps=1e-9, exactHessian=FALSE)
#' }
.extended_airline_estimation<-function(jregarima, jspec, eps=1e-9, deps=1e-9, exactHessian=FALSE){

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/core/extendedairline/LightExtendedAirlineEstimation;", "estimate",
                    jregarima, jspec, as.numeric(eps), as.numeric(deps), as.logical(exactHessian))
    return(rjd3toolkit::.jd3_object(jrslt, result=TRUE))
}

#' Log-Level Test for Extended Airline Models
#'
#' This internal function performs a statistical test to determine whether data
#' should be transformed to logarithmic scale or kept in level (original scale)
#'
#' @param jregarima Java object containing the already estimated RegARIMA model. 
#' @param jspec Java object containing the Extended Airline model specifications.
#' @param precision Numeric value specifying the tolerance for convergence
#'   of optimization algorithms. Default: 1e-5.
#' @param deps Numeric scalar. Step in the computation of the numerical derivatives,
#'   used in the optimisation routine. Default:1e-4.
#' @return An object containing the log-level test results. 
#' @export
#' 
#' @examples
#' \dontrun{
#' log_level_test_results <- .extended_airline_loglevel(jregarima, jspec, precision=1e-5)
#' }
.extended_airline_loglevel<-function(jregarima, jspec, precision=1e-5, deps = 1e-4){
    rslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "[D", "logLevelTest",
                    jregarima, jspec, as.numeric(precision), as.numeric(deps))

}

#' Outlier Detection for Extended Airline Models
#'
#' This internal function performs automatic outlier detection in high-frequency 
#' time series using Extended Airline models.
#'
#' @param jregarima Java object containing the estimated RegARIMA model.
#' @param jspec Java object containing the Extended Airline model specifications.
#' @param types Character vector specifying the types of outliers to detect.
#'   Default: c("ao") (additive outliers only). Common options include:
#'   \itemize{
#'     \item "ao" - Additive Outlier
#'     \item "ls" - Level Shift
#'     \item "wo" - Switch Outlier
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
#' @param jspec Java object containing the Extended Airline model specifications.
#' @param precision Numeric value specifying the tolerance for convergence
#'   of optimization algorithms. Default: 1e-5.
#' @param deps Numeric scalar. Step in the computation of the numerical derivatives,
#'   used in the optimisation routine. Default:1e-4.
#' @return A numeric matrix with dimensions [number of outliers detected × 2]:
#' @export
#' 
#' @examples
#' \dontrun{
#' outliers_window <- .extended_airline_outliers(
#'   jreg, jspec,
#'   types = c("ao", "ls"),
#'   start = 100,
#'   end = 500,
#'   critical_value = 6.0
#' )
#' }
.extended_airline_outliers<-function(jregarima, jspec, types=c("ao"), start=0, end=0, critical_value=0, max_outliers=30, max_round=30, precision=1e-5, deps=1e-4){
    if (start != 0) start<-start-1
    if (end != 0) end<-end-1
    rslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "outliers",
                   jregarima, jspec, .jarray(types), as.integer(start), as.integer(end),
                   as.numeric(critical_value), as.integer(max_outliers), as.integer(max_round), as.numeric((precision), as.numeric(deps)))
    return(rjd3toolkit::.jd2r_matrix(rslt)+1)
}
