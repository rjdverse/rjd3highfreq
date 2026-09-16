# internal helper to neatly print decomposition dataframes
.print_decomposition <- function(d, digits) {
    headers <- colnames(d)
    out <- rbind(utils::head(d), utils::tail(d))
    txt <- as.data.frame(lapply(out, sprintf, fmt=paste0("%.", digits, "f")))
    widths <- pmax(nchar(headers), sapply(txt, function(x) max(nchar(x))))
    format_row <- function(x) cat("  ", paste(sprintf(paste0("%", widths, "s"), x), collapse = "  "), "\n")
    format_row(headers)
    for (i in 1:5) format_row(txt[i,])
    format_row("...")
    for (i in (nrow(txt)-5):nrow(txt)) {
        format_row(txt[i,])
    }
}

# internal helper to neatly print list outputs
.print_list <- function(l, digits) {
    if (!is.null(l$series_time)) {
    l$series_time <- c(as.character(utils::head(l$series_time, 1)),
                       " ...",
                       as.character(utils::tail(l$series_time, 1))
                       )
    }

    for (name in names(l)) {
        value <- l[[name]]
        if (is.matrix(value)) {
            value_text <- paste(
                apply(value, 1, function(row) {
                    paste0(
                        "[",
                        paste(sprintf(paste0("%.", digits, "f"), row), collapse = ", "),
                        "]"
                    )
                }),
                collapse = ", "
            )
        } else if (is.numeric(value)) {
            value_text <- paste(sprintf(value, fmt=paste0("%.", digits, "f")), collapse = ", ")
        } else {
            value_text <- paste(value, collapse = ", ")
        }
        if (nchar(value_text) > 0) {
            cat(sprintf("  %-16s : %s\n", name, value_text))
        }
    }
}

# internal helper to neatly print model outputs
.print_model <- function(m, digits = 3L) {
    # Nothing to print if coefficients are missing/empty
    if (is.null(m) || is.null(m$b) || length(m$b) == 0) {
        return(invisible(NULL))
    }

    b <- m$b

    # Handle possible matrix/data.frame coefficient storage
    if (is.matrix(b) || is.data.frame(b)) {
        if (ncol(b) == 1L) {
            b_names <- rownames(b)
            b <- b[, 1L, drop = TRUE]
        } else if (nrow(b) == 1L) {
            b_names <- colnames(b)
            b <- b[1L, , drop = TRUE]
        } else {
            stop("The matrix of coefficients has more than one row and one column; cannot determine coefficients.")
        }
    } else {
        # b_names <- names(b)
        b_names <- unlist(m$variables)
    }

    b <- as.numeric(b)

    if (length(b) == 0) {
        return(invisible(NULL))
    }

    if (is.null(b_names) || length(b_names) != length(b)) {
        b_names <- paste0("b", seq_along(b))
    }

    # Calculate standard errors and t-statistics
    se <- sqrt(diag(m$bcov))
    t_stats <- b / se

    out <- cbind(
        coef = b,
        t = t_stats
    )

    rownames(out) <- b_names

    # Format values
    coef_text <- sprintf(paste0("%.", digits, "f"), out[, "coef"])
    t_text    <- sprintf(paste0("%.", digits, "f"), out[, "t"])

    # Column widths
    name_width <- max(nchar(b_names), 1L)
    coef_width <- max(nchar("coef"), nchar(coef_text))
    t_width <- max(nchar("t"), nchar(t_text))

    # Header
    cat(sprintf(
        "  %-*s  %*s  %*s\n",
        name_width, "",
        coef_width, "coef",
        t_width, "t"
    ))

    # Separator
    cat(sprintf(
        "  %-*s  %*s  %*s\n",
        name_width, paste(rep("-", name_width), collapse = ""),
        coef_width, paste(rep("-", coef_width), collapse = ""),
        t_width, paste(rep("-", t_width), collapse = "")
    ))

    # Rows
    for (i in seq_along(b)) {
        cat(sprintf(
            "  %-*s  %*s  %*s\n",
            name_width, b_names[i],
            coef_width, coef_text[i],
            t_width, t_text[i]
        ))
    }

    invisible(out)
}

#' Print method for hf_decomposition objects
#'
#' @param x An object of class \code{hf_decomposition}.
#' @param digits Number of significant digits for numeric output.
#' @param ...  Ignored.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @exportS3Method print hf_decomposition

print.hf_decomposition <- function(x, digits = 3L, ...) {
    cat("\nDecomposition:\n\n")
    .print_decomposition(x$decomposition, digits)
    cat("\n\nParameters:\n\n")
    .print_list(x$parameters, digits)
    cat("\n\n")
    return(invisible(x))
}


#' Print method for hf_estimation objects
#'
#' @param x   An object of class \code{hf_estimation}.
#' @param digits  Number of significant digits for numeric output.
#' @param ...  Ignored.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @exportS3Method print hf_estimation

print.hf_estimation <- function(x, digits = 3L, ...) {
    cat("\n\nFractional Airline estimation:\n\n")
    .print_list(x$estimation, digits)
    cat("\n\nRegression results:\n\n")
    .print_model(x$model, digits)
    cat("\n\nLikelihood:\n\n")
    .print_list(x$likelihood, digits)
    cat("\n\n")
    return(invisible(x))
}
