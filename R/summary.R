#' Summary method for hf_decomposition objects
#'
#' @param object An object of class \code{hf_decomposition}.
#' @param digits Number of significant digits for numeric output.
#' @param ...  Ignored.
#' @return \code{object}, invisibly.
#' @keywords internal
#' @exportS3Method summary hf_decomposition

summary.hf_decomposition <- function(object, digits = 3L, ...) {
    print(object, digits)
    return(invisible(object))
}


#' Summary method for JDFractionalAirlineEstimation objects
#'
#' @param object   An object of class \code{JDFractionalAirlineEstimation}.
#' @param digits  Number of significant digits for numeric output.
#' @param ...  Ignored.
#' @return \code{object}, invisibly.
#' @keywords internal
#' @exportS3Method summary hf_estimation

summary.hf_estimation <- function(object, digits = 3L, ...) {
    print(object, digits)
    return(invisible(object))
}
