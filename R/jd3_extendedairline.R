#' @include utils.R
NULL

#' Internal routine to create an ExtendedAirlineSpec
#'
#' @param periodicities Periodicities
#' @param differencing Differnecing order. -1 for automatic computation
#' @param ar Use of an AR regular stationary polynomial instead of a MA polynomial
#' @param toint Round periodicties to integers
#'
#' @return A Java ExtendedAirlineSpec object
#' @export
#'
#' @examples .extendedairline_spec(c(7, 365.25))
.extendedairline_spec<-function(periodicities, differencing=-1, ar=FALSE, toint=FALSE){

    if (differencing == -1){
        differencing = length(periodicities)
        if (! ar) differencing=differencing + 1

    }

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/api/ExtendedAirlineSpec;", "spec",
                 .jarray(as.numeric(periodicities)), as.integer(differencing), as.logical(ar), as.logical(toint))
    return(jrslt)
}

#' Creates a java RegArima models based on an extended airline spec
#'
#' @param y y
#' @param jspec Java spec
#' @param mean Mean correction (to be avoided)
#' @param X Regression variables
#'
#' @return A Java RegArima model
#' @export
#'
#' @examples
#' jspec<-.extendedairline_spec(c(12))
#' .extendedairline_regarima(rjd3toolkit::ABS$X0.2.09.10.M, jspec)
.extendedairline_regarima<-function(y, jspec, mean=FALSE, X=NULL){

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/toolkit/base/core/regarima/RegArimaModel;", "regarima",
                    as.numeric(y), as.logical(mean), rjd3toolkit::.r2jd_matrix(X), jspec)
    return(jrslt)
}

#' Title
#'
#' @param jregarima
#' @param jspec
#' @param eps
#' @param exactHessian
#'
#' @return
#' @export
#'
#' @examples
.extendedairline_estimation<-function(jregarima, jspec, eps=1e-9, exactHessian=FALSE){

    jrslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/highfreq/base/core/extendedairline/LightExtendedAirlineEstimation;", "estimate",
                    jregarima, jspec, as.numeric(eps), as.logical(exactHessian))
    return(rjd3toolkit::.jd3_object(jrslt, result=TRUE))
}

#' Title
#'
#' @param jregarima
#' @param jspec
#' @param precision
#'
#' @return
#' @export
#'
#' @examples
.extended_airline_loglevel<-function(jregarima, jspec, precision=1e-5){
    rslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "[D", "logLevelTest",
                    jregarima, jspec, as.numeric(precision))
    return(rslt)

}

#' Title
#'
#' @param jregarima
#' @param jspec
#' @param types
#' @param start
#' @param end
#' @param critical_value
#' @param max_outliers
#' @param max_round
#'
#' @return
#' @export
#'
#' @examples
.extended_airline_outliers<-function(jregarima, jspec, types=c("ao"), start=0, end=0, critical_value=0, max_outliers=30, max_round=30){
    if (start != 0) start=start-1
    if (end != 0) end=end-1
    rslt <- .jcall("jdplus/highfreq/base/r/ExtendedAirlineProcessor", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "outliers",
                   jregarima, jspec, .jarray(types), as.integer(start), as.integer(end),
                   as.numeric(critical_value), as.integer(max_outliers), as.integer(max_round) )
    return(rjd3toolkit::.jd2r_matrix(rslt)+1)
}


