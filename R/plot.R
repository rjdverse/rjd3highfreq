
#' Create a custom plot replicating the JD+ GUI visual style
#'
#' This internal function generates a time series plot that replicates the visual
#' template of the JD+ graphical user interface. The function 
#' supports plotting one or more time series simultaneously.
#' 
#' @param x Numeric vector representing the x-axis values, typically a R time 
#'   series object.
#' @param y A list of numeric vectors, where each element represents a
#'   different time series to be plotted on the y-axis.
#' @param col A character or color vector specifying the color assigned to each
#'   series.
#' @param legend_txt An optional character vector of labels for the legend.
#' @param ... Additional graphical parameters. Can be used to customize line
#'   types (\code{lty}), line widths (\code{lwd}), or other aesthetic properties
#'   that apply uniformly to all series.
#'
#' @return `NULL` (invisible).
#' 
#' @export
.plot_jd <- function(x, y, col, legend_txt = NULL, ...) {

    col_bg <- "#f5f4e7"
    col_grid <-"#dadad3"
    y_range <- range(do.call(c, y))

    plot.new()
    rect(xleft = par("usr")[1], xright = par("usr")[2],
         ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
    par(new = TRUE)
    plot(y = y[[1]], x = x,
         col = col[1], type = "l", xlab = "", ylab = "", ylim = y_range,
         main = "", xaxt = "n", yaxt = "n")
    x_breaks <- Axis(x, side = 1)
    par(xaxp = c(x_breaks[1], x_breaks[length(x_breaks)], length(x_breaks) - 1))
    grid(nx = NULL, ny = NULL, col = col_grid)
    par(new = TRUE)

    plot(y = y[[1]],
         x = x,
         col = col[1], type = "l", xlab = "Time", ylim = y_range,
         xaxt = "n", ...)

    for (k in (seq_along(y[-1]) + 1)) {
        lines(y = y[[k]],
              x = x,
              col = col[k], ...)
    }

    box(col = col_grid)

    if (!is.null(legend_txt)) {
        legend("bottomleft", legend = legend_txt,
               pch = 16, col = col, horiz = TRUE, xpd = TRUE,
               inset = c(0, 1), bty = "n")
    }

    return(invisible(NULL))
}

#' Plot method for JDFractionalAirlineEstimation objects
#'
#' This function implements the \code{\link[graphics]{plot}} S3 method for objects
#' of class \code{JDFractionalAirlineEstimation}.
#' 
#' @param x An object of class \code{JDFractionalAirlineEstimation}, as
#'   returned by the fractional airline estimation functions.
#' @param from An optional \code{Date} or \code{POSIXt} object specifying
#'   the start of the time window to display.
#' @param to An optional \code{Date} or \code{POSIXt} object specifying
#'   the end of the time window to display.
#' @param ... Additional graphical parameters passed to \code{\link{.plot_jd}}.
#'
#' @return `NULL` (invisible).
#'
#' @examples
#' \dontrun{
#' # Assuming 'estimation' is a JDFractionalAirlineEstimation object
#' # Basic plot: raw data vs. linearised series
#' plot(estimation)
#'
#' # Plot restricted to a specific time window
#' plot(estimation,
#'      from = as.Date("2015-01-01"),
#'      to   = as.Date("2020-12-31"))
#'
#' # Combining time window, custom title and line width
#' plot(estimation,
#'      from = as.Date("2018-06-01"),
#'      to   = as.Date("2023-01-01"),
#'      main = "Filtered window",
#'      lwd  = 2)
#' }
#'
#' @export
plot.JDFractionalAirlineEstimation <- function(x, from, to, ...) {

    col_y <- "#f1ba1d"
    col_t <- "#1e6c0b"
    col_sa <- "#00488c"

    col_s1 <- "#ffab78"
    col_s2 <- "#9169be"

    y <- x$model$y
    y_lin <- x$model$linearized
    if (x$model$log) {
        y_lin <- exp(y_lin)
    }

    vect_x <- x$model$y_time
    if (is.null(vect_x)) {
        vect_x <- seq_along(y)
    } else {
        if (!missing(from)) {
            vect_x <- vect_x[vect_x >= from]
        }
        if (!missing(to)) {
            vect_x <- vect_x[vect_x <= to]
        }
        y <- y[which(x$model$y_time %in% vect_x)]
        y_lin <- y_lin[which(x$model$y_time %in% vect_x)]
    }

    list_args <- list(...)
    list_args$main <- ifelse("main" %in% names(list_args),
                             yes = paste0("Raw data and linearised series", " - ", list_args$main),
                             no = "Raw data and linearised series")
    list_args$ylab <- ifelse("ylab" %in% names(list_args),
                             yes = list_args$ylab, no = "")
    list_args$col <- c(col_y, col_t)

    do.call(.plot_jd,
            c(list(
                x = vect_x, y = list(y, y_lin),
                legend_txt = c("Raw data", "Linearised series")),
              list_args)
    )

    return(invisible(NULL))
}

#' Plot method for JDFractionalAirlineDecomposition objects
#'
#' This function implements the \code{\link[graphics]{plot}} S3 method for objects
#' of class \code{JDFractionalAirlineDecomposition}. It produces one or two
#' JD+-styled plots (via \code{\link{.plot_jd}}).
#'
#' @param x An object of class \code{JDFractionalAirlineDecomposition}, as
#'   returned by the fractional airline decomposition functions.
#' @param from An optional \code{Date} or \code{POSIXt} object specifying
#'   the start of the time window to display.
#' @param to An optional \code{Date} or \code{POSIXt} object specifying
#'   the end of the time window to display.
#' @param type_chart Character vector specifying which chart(s) to render.
#'   Accepts one or both of the following values:
#'   \itemize{
#'     \item \code{"y-sa-trend"} — plots the raw data, seasonally adjusted
#'         series, and trend-cycle component (default)
#'     \item \code{"cal-seas-irr"} — plots all seasonal components and the
#'         irregular component
#'   }
#' @param ... Additional graphical parameters passed through to 
#' \code{\link{.plot_jd}}.
#'    
#' @return `NULL` (invisible).
#' @examples
#' \dontrun{
#' # Assuming 'decomp' is a JDFractionalAirlineDecomposition object
#'
#' # --- Default: render both charts sequentially ---------------------------
#' plot(decomp)
#'
#' # --- Render only the raw / SA / trend chart -----------------------------
#' plot(decomp, type_chart = "y-sa-trend")
#'
#' # --- Restrict both charts to a specific time window ----------------------
#' plot(decomp,
#'      from = as.Date("2016-01-01"),
#'      to   = as.Date("2022-12-31"))
#'
#' # --- Both charts stacked vertically with a custom line width --------------
#' par(mfrow = c(2, 1))
#' plot(decomp,
#'      from = as.Date("2018-01-01"),
#'      to   = as.Date("2023-06-30"),
#'      main = "Stacked view",
#'      lwd  = 2)
#' par(mfrow = c(1, 1))   # reset layout
#' }
#' 
#' @export
plot.JDFractionalAirlineDecomposition <- function(
        x, from, to, type_chart = c("y-sa-trend", "cal-seas-irr"), ...) {

    if ("y-sa-trend" %in% type_chart) {

        col_y <- "#f1ba1d"
        col_t <- "#1e6c0b"
        col_sa <- "#00488c"

        y <- x$decomposition$y
        sa <- x$decomposition$sa
        tc <- x$decomposition$t
        if (x$estimation$log) {
            y <- exp(y)
            sa <- exp(sa)
            tc <- exp(tc)
        }

        vect_x <- x$decomposition$y_time
        if (is.null(vect_x)) {
            vect_x <- seq_along(y)
        } else {
            if (!missing(from)) {
                vect_x <- vect_x[vect_x >= from]
            }
            if (!missing(to)) {
                vect_x <- vect_x[vect_x <= to]
            }
            time_lim <- which(x$decomposition$y_time %in% vect_x)
            y <- y[time_lim]
            sa <- sa[time_lim]
            tc <- tc[time_lim]
        }

        list_args <- list(...)
        list_args$main <- ifelse("main" %in% names(list_args),
                                 paste0("Decomposition AMB", " - ", list_args$main),
                                 "Decomposition AMB")
        list_args$ylab <- ifelse("ylab" %in% names(list_args),
                                 yes = list_args$ylab, no = "")
        list_args$col <- c(col_y, col_sa, col_t)

        do.call(.plot_jd,
                c(list(
                    x = vect_x, y = list(y, sa, tc),
                    legend_txt = c("Raw data", "Seasonnal adjusted", "Trend")),
                  list_args)
        )
    }

    if ("cal-seas-irr" %in% type_chart) {

        col_s <- c("#ffab78", "#9169be", "#4d8076", "#c34a36", "#00c9a7")
        s_variables <- names(x$decomposition)
        s_variables <- s_variables[grepl("^s(?!a)", s_variables, perl = TRUE)]

        s <- x$decomposition[s_variables]
        ic <- x$decomposition$i
        if (x$estimation$log) {
            s <- lapply(X = s, FUN = exp)
            ic <- exp(ic)
        }

        vect_x <- x$decomposition$y_time
        if (is.null(vect_x)) {
            vect_x <- seq_along(ic)
        } else {
            if (!missing(from)) {
                vect_x <- vect_x[vect_x >= from]
            }
            if (!missing(to)) {
                vect_x <- vect_x[vect_x <= to]
            }
            time_lim <- which(x$decomposition$y_time %in% vect_x)

            s <- lapply(X = s, FUN = base::`[`, time_lim)
            ic <- ic[time_lim]
        }

        list_args <- list(...)
        list_args$main <- ifelse("main" %in% names(list_args),
                                 paste0("Irregular and Seasonal components", " - ", list_args$main),
                                 "Irregular and Seasonal components")
        list_args$ylab <- ifelse("ylab" %in% names(list_args),
                                 yes = list_args$ylab, no = "")
        list_args$col <- col_s[seq_len(length(s) + 1)]

        do.call(.plot_jd,
                c(list(
                    x = vect_x, y = c(s, list(ic)),
                    legend_txt = c(s_variables, "Irregular")),
                  list_args)
        )
    }

    return(invisible(NULL))
}
