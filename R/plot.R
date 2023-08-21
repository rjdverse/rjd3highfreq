
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
  if ("main" %in% names(list_args)) {
    list_args$main <- paste0("Raw data and linearised series", " - ", list_args$main)
  }
  
  do.call(plot_jd, 
          c(list(
            x = vect_x, y = list(y, y_lin), 
            col = c(col_y, col_t), 
            legend_txt = c("Raw data", "Linearised series"), 
            ylab = ""), 
            list_args)
  )
  
  return(invisible(NULL))
}

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
    if ("main" %in% names(list_args)) {
      list_args$main <- paste0("Decomposition AMB", " - ", list_args$main)
    }
    
    do.call(plot_jd, 
            c(list(
              x = vect_x, y = list(y, sa, tc), 
              col = c(col_y, col_sa, col_t), 
              legend_txt = c("Raw data", "Seasonnal adjusted", "Trend"), 
              ylab = ""), 
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
    if ("main" %in% names(list_args)) {
      list_args$main <- paste0("Irregular and Seasonal components", " - ", list_args$main)
    }
    
    do.call(plot_jd, 
            c(list(
              x = vect_x, y = c(s, list(ic)), 
              col = col_s[seq_len(length(s) + 1)], 
              legend_txt = c(s_variables, "Irregular"), 
              ylab = ""), 
              list_args)
    )
  }
  
  return(invisible(NULL))
}