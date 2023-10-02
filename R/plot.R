
#' Custom Plot Function on JD+ template
#'
#' This function creates a customized plot in the same template as JD+ GUI color and forms.
#' 
#' @param x Numeric vector, x-axis values.
#' @param y List of numeric vectors, y-axis values for different series.
#' @param col Vector of colors for different series.
#' @param legend_txt Character vector of legend labels for different series.
#' @param ... Additional graphical parameters.
#'
#' @return `NULL` (invisible).
#'
plot_jd <- function(x, y, col, legend_txt = NULL, ...){
  
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

#' Plot Function for JDFractionalAirlineEstimation Objects
#'
#' This function creates a plot for the result of fractional airline model (class `JDFractionalAirlineEstimation`). It shows the raw data and linearized series.
#' 
#' @param x An object of class 'JDFractionalAirlineEstimation'.
#' @param from `Date` or `POSIXt` object, optional starting point for x-axis.
#' @param to `Date` or `POSIXt` object, optional ending point for x-axis.
#' @param ... Additional graphical parameters.
#'
#' @return `NULL` (invisible).
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
  
  do.call(plot_jd, 
          c(list(
            x = vect_x, y = list(y, y_lin), 
            legend_txt = c("Raw data", "Linearised series")), 
            list_args)
  )
  
  return(invisible(NULL))
}

#' Plot Function for JDFractionalAirlineDecomposition Objects
#'
#' This function creates a plot for the result of an Arima Model Based (AMB) decomposition of one or several frequencies (class `JDFractionalAirlineDecomposition`). It shows the decomposition and the component of the model.
#' 
#' 
#' @param x An object of class 'JDFractionalAirlineDecomposition'.
#' @param from `Date` or `POSIXt` object, optional starting point for x-axis.
#' @param to `Date` or `POSIXt` object, optional ending point for x-axis.
#' @param type_chart Character vector specifying the type of chart to plot ("y-sa-trend", "cal-seas-irr").
#' @param ... Additional graphical parameters.
#'
#' @return `NULL` (invisible).
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
    
    do.call(plot_jd, 
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
    
    do.call(plot_jd, 
            c(list(
              x = vect_x, y = c(s, list(ic)), 
              legend_txt = c(s_variables, "Irregular")), 
              list_args)
    )
  }
  
  return(invisible(NULL))
}
