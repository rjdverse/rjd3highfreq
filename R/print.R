#' Print method for 'JDFractionalAirlineDecomposition' objects
#'
#' This function prints informations on the result of a Fractional Airline model (classe JDFractionalAirlineDecomposition).
#'
#' @param x An object of class 'JDFractionalAirlineDecomposition'.
#' @param digits Number of digits to round numerical values (default is 3 or digits - 3 from options).
#'
#' @return The original object 'x'.
#'
#' @export
print.JDFractionalAirlineDecomposition <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("Number of observations:", formatC(x$likelihood$nobs, digits = digits))
  cat("\n")

  if (!is.null(x$decomposition$y_time)) {
    cat("Start:", format(x$decomposition$y_time[1]),"\n")
    cat("End:", format(x$decomposition$y_time[length(x$decomposition$y_time)]), "\n")
  }

  # Estimated MA parameters (coefs, se, student)
  nb_freq <- length(x$estimation$parameters) - 1L
  est_ma_params <- data.frame(
    MA_parameter = c("Theta(1)",
                     paste0("Theta(", paste0("period = ",
                                             x$estimation$periods), ")")),
    Coef = x$estimation$parameters,
    Coef_SE = sqrt(diag(x$estimation$covariance)),
    check.names = FALSE)
  est_ma_params$Tstat <- est_ma_params$Coef / est_ma_params$Coef_SE

  cat("\n")
  cat("Estimate MA parameters:")
  cat("\n")
  print(est_ma_params, row.names = FALSE)
  cat(ifelse(x$estimation$log, "Multiplicative", "Additive"), "model\n")

  cat("\n")
  cat("Decomposition:")
  cat("\n")
  decompo_table <- do.call(cbind, x$decomposition)
  if (x$estimation$log) {
    decompo_table <- exp(decompo_table)
  }

  if (!is.null(x$decomposition$y_time)) {
    decompo_table <- subset(decompo_table, select = -y_time)
    rownames(decompo_table) <- format(x$decomposition$y_time)
  }
  print(tail(decompo_table, n = 10))
  cat("\n")

  cat("Sum of square residuals:", formatC(x$likelihood$ssq, digits = digits),
      "on", x$likelihood$df, "degrees of freedom",
      sep = " ")
  cat("\n")

  cat("Log likelihood = ", formatC(x$likelihood$ll, digits = digits),
      ", \n\taic = ", formatC(x$likelihood$aic, digits = digits),
      ", \n\taicc = ", formatC(x$likelihood$aicc, digits = digits),
      ", \n\tbic(corrected for length) = ",
      formatC(x$likelihood$bicc, digits = digits), sep = "")
  cat("\n")

  cat("Hannan–Quinn information criterion = ",
      formatC(x$likelihood$hannanquinn, digits = digits), sep = "")

  cat("\n\n")

  return(invisible(x))
}

#' Print method for 'JDFractionalAirlineEstimation' objects
#'
#' This function prints informations on the result of a Fractional Airline model (classe JDFractionalAirlineEstimation).
#'
#' @param x An object of class 'JDFractionalAirlineEstimation'.
#' @param digits Number of digits to round numerical values (default is 3 or digits - 3 from options).
#'
#' @return The original object 'x'.
#'
#' @export
print.JDFractionalAirlineEstimation <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  cat("Number of observations:", formatC(x$likelihood$nobs, digits = digits))
  cat("\n")

  if (!is.null(x$model$y_time)) {
    cat("Start:", format(x$model$y_time[1]),"\n")
    cat("End:", format(x$model$y_time[length(x$model$y_time)]), "\n")
  }



  nb_outliers <- sum(toupper(substr(x$model$variables, 1L, 2L)) %in%
                       c("AO", "WO", "LS"))
  nb_reg_cjo <- length(x$model$variables) - nb_outliers

  summary_coeff <-  data.frame(
    "Variable" = x$model$variables,
    "Coef" = x$model$b,
    "Coef_SE" = sqrt(diag(x$model$bcov)))
  summary_coeff$Tstat <- round(summary_coeff$Coef / summary_coeff$Coef_SE, digits)
  summary_coeff$Coef <- round(summary_coeff$Coef, digits)
  summary_coeff$Coef_SE <- round(summary_coeff$Coef_SE, digits)

  if (nb_outliers > 0) {
    outliers_coeff <- summary_coeff[(nb_reg_cjo + 1L):nrow(summary_coeff), ]

    if (!is.null(x$model$y_time)) {
      outliers_type <- substr(outliers_coeff$Variable, start = 1, stop = 2)
      outliers_date <- x$model$y_time[as.integer(substr(outliers_coeff$Variable, start = 4, stop = 50))]
      outliers_coeff$Variable <- paste0(outliers_type, ".", outliers_date)
    }
  }

  if (nb_reg_cjo > 0) {
    reg_cjo_coeff <- summary_coeff[seq_len(nb_reg_cjo), ]
  }

  # Estimated MA parameters (coefs, se, student)
  nb_freq <- length(x$estimation$parameters) - 1L
  est_ma_params <- data.frame(
    MA_parameter = c("Theta(1)",
                     paste0("Theta(", paste0("period = ",
                                             x$model$periods), ")")),
    Coef = x$estimation$parameters,
    Coef_SE = sqrt(diag(x$estimation$covariance)),
    check.names = FALSE)
  est_ma_params$Tstat <- est_ma_params$Coef / est_ma_params$Coef_SE

  cat("\n")
  cat("Estimate MA parameters:")
  cat("\n")
  print(est_ma_params, row.names = FALSE)

  cat("\n")
  cat("Number of calendar regressors:", nb_reg_cjo, ", Number of outliers :", nb_outliers)
  cat("\n\n")

  if(nb_reg_cjo > 0) {
    cat("TD regressors coefficients:")
    cat("\n")
    print(reg_cjo_coeff, row.names = FALSE)
    # print(head(reg_cjo_coeff, 10), row.names = FALSE)
    # if (nb_reg_cjo > 10) cat("...\n")
    cat("\n")
  }

  if(nb_outliers > 0) {
    cat("Outliers coefficients:")
    cat("\n")
    print(outliers_coeff, row.names = FALSE)
    # print(head(outliers_coeff, 10), row.names = FALSE)
    # if (nb_outliers > 10) cat("...\n")
    cat("\n")
  }

  cat("Sum of square residuals:", formatC(x$likelihood$ssq, digits = digits),
      "on", x$likelihood$df, "degrees of freedom",
      sep = " ")
  cat("\n")

  cat("Log likelihood = ", formatC(x$likelihood$ll, digits = digits),
      ", \n\taic = ", formatC(x$likelihood$aic, digits = digits),
      ", \n\taicc = ", formatC(x$likelihood$aicc, digits = digits),
      ", \n\tbic(corrected for length) = ",
      formatC(x$likelihood$bicc, digits = digits), sep = "")
  cat("\n")

  cat("Hannan–Quinn information criterion = ",
      formatC(x$likelihood$hannanquinn, digits = digits), sep = "")

  cat("\n\n")
  return(invisible(x))
}
