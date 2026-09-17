# internal helper to draw empty canvas
.canvas <- function(series, title) {
    plot(
        series,
        type = "n",
        xlab = "",
        ylab = "",
        axes = FALSE,
        frame.plot = FALSE,
        main = title
    )
}


# internal helper to draw nice axes
.nice_axes <- function(col) {
    graphics::axis(
        side = 1,
        col.axis = col,
        lwd = 0,
        lwd.ticks = 0,
        las = 1
    )
    graphics::axis(
        side = 2,
        col.axis = col,
        lwd = 0,
        lwd.ticks = 0,
        las = 1
    )
}


#' Plot method for hf_decomposition objects
#'
#' @param x An object of class \code{hf_decomposition}.
#' @param ... options for par()
#' @param col_bg Background color.
#' @param col_grid Color of grid lines.
#' @param col_labels Color of tick labels.
#' @param col_1 First time series color.
#' @param col_2 Second time series color.
#' @param col_3 Third time series color.
#' @keywords internal
#' @exportS3Method plot hf_decomposition

plot.hf_decomposition <- function(
        x,
        ...,
        col_bg     = "white",
        col_grid   = "#777777",
        col_labels = "#777777",
        col_1      = "#debc12",
        col_2      = "#74b6d6",
        col_3      = "#ee2c2c"
) {

    names_without_series_and_sa <- names(x$decomposition)[
        !names(x$decomposition) %in% c("series", "sa", "series_time")
    ]

    seasonal_names <- names_without_series_and_sa[
        substr(names_without_series_and_sa, 1, 1) == "s"
    ]

    n_right <- length(seasonal_names) + 1L

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
        graphics::layout(1)
        graphics::par(old_par)
    })

    layout_matrix <- cbind(
        rep(1L, n_right),
        seq_len(n_right) + 1L
    )

    graphics::layout(
        layout_matrix,
        widths  = c(1.25, 1),
        heights = rep(1, n_right)
    )

    graphics::par(
        bg   = col_bg,
        oma  = c(0.6, 0.4, 0.4, 0.4),
        mgp  = c(1.6, 0.45, 0),
        tcl  = -0.25,
        xaxs = "i",
        yaxs = "i",
        ...
    )

    ## Left large graphic
    graphics::par(
        mar = c(2.8, 3.2, 2.2, 0.8)
    )

    .canvas(
        x$decomposition$series,
        title = "Decomposition results"
    )
    graphics::grid(col = col_grid)
    .nice_axes(col = col_labels)

    graphics::lines(x$decomposition$series, col = col_1, lwd = 1.5)
    graphics::lines(x$decomposition$sa,     col = col_2, lwd = 2)
    graphics::lines(x$decomposition$t,      col = col_3, lwd = 2)

    graphics::legend(
        "topleft",
        inset = 0.03,
        legend = c("Series", "Adjusted", "Trend"),
        lwd = c(2, 2, 2),
        box.lty = 0,
        col = c(col_1, col_2, col_3),
        bg = col_bg
    )

    ## Right side: slightly more space between graphics
    graphics::par(
        mar = c(2.4, 3.0, 2.0, 0.8)
    )

    for (s_name in seasonal_names) {
        s <- x$decomposition[, s_name]

        if (length(seasonal_names) == 1) {
            title <- "Seasonal"
        } else {
            title <- paste0("Seasonal: ", s_name)
        }

        .canvas(s, title = title)
        graphics::grid(col = col_grid)
        .nice_axes(col = col_labels)
        graphics::lines(s, col = col_2, lwd = 1.5)
    }

    ## Irregular Component
    .canvas(x$decomposition$i, title = "Irregular")
    graphics::grid(col = col_grid)
    .nice_axes(col = col_labels)
    graphics::lines(x$decomposition$i, col = col_3, lwd = 1.5)

    invisible(NULL)
}


#' Plot method for hf_estimation objects
#'
#' @param x An object of class \code{JDFractionalAirlineDecomposition}.
#' @param ... additional options for first line.
#' @param col_bg Background color.
#' @param col_grid Color of grid lines.
#' @param col_labels Color of tick labels.
#' @param col_1 First time series color.
#' @param col_2 Second time series color.
#' @return \code{x}, invisibly.
#' @keywords internal
#' @exportS3Method plot hf_estimation

plot.hf_estimation <- function(
    x,
    ...,
    col_bg     = "#ffffe0",
    col_grid   = "#777777",
    col_labels = "#777777",
    col_1      = "#debc12",
    col_2      = "#74b6d6"
    ) {

    # set new par, but restore old par on exit
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))
    graphics::par(mfrow = c(2,1), bg = col_bg)

    # top plot: series
    .canvas(x$decomposition$series, title = "Time Series")
    graphics::grid(col = col_grid)
    .nice_axes(col = col_labels)
    graphics::lines(x$decomposition$series, col = col_1, lwd = 1.5, ...)

    # bottom plot: residuals
    .canvas(x$decomposition$residuals, title = "Residuals")
    graphics::grid(col = col_grid)
    .nice_axes(col = col_labels)
    graphics::lines(x$decomposition$residuals, col = col_2, lwd = 1.5)

    return(invisible(NULL))
}
