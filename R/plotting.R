#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_c
NULL

# in data_frame calls. Should be replaced with standard evaluation.
globalVariables("x")

#' Normal Distribution Density Function
#'
#' Plot area under the normal distribution density curve.
#'
#' @param xmin Minimum value for the x-axis
#' @param xmax Maximum value for the x-axis
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param n Number of points to interpolate along
#' @param ... Arguments passed to \code{\link{geom_ribbon}}.
#'
#' @family Plots of the normal distribution
#' @export
#' @examples
#' geom_dnorm_area(-2, 2)
#' mu <- 1
#' sigma <- 2
#' geom_dnorm_area(0, 2, mean = mu, sd = sigma)
geom_dnorm_area <- function(xmin, xmax, mean = 0, sd = 1, n = 101, ...) {

  .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                      ymin = 0,
                      ymax = dnorm(x, mean = mean, sd = sd))
  geom_ribbon(data = .data,
              mapping = aes_string(x = "x",
                                   ymin = "ymin",
                                   ymax = "ymax"),
              ...)
}

#' Area Under the Normal Distribution Density Function
#'
#' Plot the normal distribution density function.
#'
#' @param xmin Minimum value for the x-axis
#' @param xmax Maximum value for the x-axis
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param n Number of points to interpolate along
#' @param ... Arguments passed to \code{\link{geom_line}}.
#'
#' @family Plots of the normal distribution
#' @export
#' @examples
#' geom_dnorm_line(-4, 4)
#' mu <- 2
#' sigma <- 3
#' geom_dnorm_line(mu - 3 * sigma, mu + 3 * sigma, mean = mu, sd = sigma)
geom_dnorm_line <- function(xmin, xmax, mean = 0, sd = 1, n = 101, ...) {
  .data <-
    data_frame(x = seq(xmin, xmax, length.out = n)) %>%
    mutate(y = dnorm(x, mean, sd))
  geom_line(data = .data,
            mapping = aes_string(x = "x",
                                 y = "y"), ...)
}

#' Student's t Distribution Density Function
#'
#' Plot the Student's t distribution density function.
#'
#' @param df Degrees of freedom of the Student's t distribution.
#' @param xmin Minimum value for the x-axis
#' @param xmax Maximum value for the x-axis
#' @param mean Location of the Student's t distribution
#' @param scale Scale of the of the Student's t distribution
#' @param n Number of points to interpolate along
#' @param ... Arguments passed to \code{\link{geom_line}}.
#'
#' @family Plots of the t distribution
#' @export
geom_dt_line <- function(df, xmin, xmax, mean = 0, scale = 1, n = 101, ...) {
  if (is.infinite(df)) {
    geom_dnorm_line(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        y = dt((x - mean) / scale, df = df) * scale + mean)
    geom_line(data = .data, mapping = aes_string(x = "x", y = "y"), ...)
  }
}

#' Area under the Student's t Distribution Density Function
#'
#' Plot the area under the curve of a Student's t distribution density function.
#'
#' @param df Degrees of freedom of the Student's t distribution.
#' @param xmin Minimum value for the x-axis
#' @param xmax Maximum value for the x-axis
#' @param mean Location of the Student's t distribution
#' @param scale Scale of the of the Student's t distribution
#' @param n Number of points to interpolate along
#' @param ... Arguments passed to \code{\link{geom_line}}.
#'
#' @family Plots of the t distribution
#' @export
geom_dt_area <- function(df, xmin, xmax, mean = 0, scale = 1, n  = 101,
                         ...) {
  if (is.infinite(df)) {
    geom_dnorm_area(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        ymin = 0,
                        ymax = dt((x - mean) / scale, df = df) * scale + mean)
    geom_ribbon(data = .data,
                mapping = aes_string(x = "x", ymin = "ymin", ymax = "ymax"),
                ...)
  }
}

#' Normal Distribution Density Plot
#'
#' Plot of the normal distribution density function, with typical defaults used
#' in introducing normal distributions.
#'
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param max.sd The x-axis of the plot is \code{mean + c(-1, 1) * max.sd * sd }.
#' @param n Number of points to interpolate along
#' @param digits Number of digits to display in x-axis breaks.
#' @param ... Arguments passed to \code{\link{geom_dnorm_line}}.
#'
#' @family Plots of the normal distribution
#' @export
normal_plot <- function(mean = 0, sd = 1, max.sd = 4, n = 101, digits = 2, ...) {
  limits <- mean + max.sd * c(-1, 1) * sd
  x <- seq(limits[1], limits[2], length.out = n)
  breaks <- seq(mean - max.sd * sd,
                mean + max.sd * sd,
                by = sd)
  (ggplot()
   + geom_dnorm_line(xmin = limits[1],
                     xmax = limits[2],
                     mean = mean, sd = sd, ...)
   + scale_x_continuous("x", limits = limits,
                        breaks = round(breaks, digits))
   + scale_y_continuous("p(x)"))
}

#' Normal Distribution Density Plot with Tail Areas
#'
#' Plot of the normal distribution density function, with typical defaults used
#' in introducing normal distributions, and tail areas shaded.
#'
#' @param q Quantile to use for the tails
#' @param p Probabilities of the tail areas
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#' @param lower.tail logical; if \code{TRUE}, probabilities are \eqn{P(X \leq x)},
#'   else probabilities are \eqn{P(X > x)}.
#' @param two.sided logical; if \code{TRUE}, two-sided tails. The tails are are
#'   at \code{c(-q, q)} if qunatiles are used, and at the quantiles such that
#'   \eqn{P(X \leq x) = p} and \eqn{P(X > x)} if probabilities are used.
#' @param max.sd The x-axis of the plot is \code{mean + c(-1, 1) * max.sd * sd }.
#' @param area_opts list. Arguments passed to \code{\link{geom_dnorm_area}}.
#' @param ... Arguments passed to \code{\link{geom_dnorm_line}}.
#'
#' @family Plots of the normal distribution
#' @rdname normal_tail_plot_q
#' @export
normal_tail_plot_q <- function(q, mean = 0, sd = 1,
                               lower.tail = TRUE, two.sided = FALSE,
                               area_opts = list(),
                               max.sd = 4,
                               ...) {
  limits <- mean + max.sd * c(-1, 1) * sd

  gg <- normal_plot(mean = mean, sd = sd, ...)
  if (! two.sided) {
    if (lower.tail) {
      if (q > limits[1]) {
        gg <- gg + do.call(geom_dnorm_area, c(list(limits[1], q, mean, sd),
                                              area_opts))
      }
    } else {
      if (q < limits[2]) {
        gg <- gg +  do.call(geom_dnorm_area, c(list(q, limits[2], mean, sd),
                                               area_opts))
      }
    }
  } else {
    q <- c(-1, 1) * abs((q - mean) / sd) * sd + mean
    if (q[1] > limits[1]) {
      gg <- gg + do.call(geom_dnorm_area, c(list(limits[1], q[1], mean, sd),
                                            area_opts))
    }
    if (q[2] < limits[2]) {
      gg <- gg + do.call(geom_dnorm_area, c(list(q[2], limits[2], mean, sd),
                                            area_opts))
    }
  }
  gg
}

#' @rdname normal_tail_plot_q
#' @export
normal_tail_plot_p <- function(p, mean = 0, sd = 1,
                               lower.tail = TRUE, two.sided = FALSE,
                               ...) {
  if (! two.sided) {
    q <- qnorm(p, mean, sd, lower.tail = lower.tail)
  } else {
    q <- qnorm(p / 2, mean, sd, lower.tail = TRUE)
  }
  normal_tail_plot_q(q, mean = mean, sd = sd, lower.tail = lower.tail,
                     two.sided = two.sided, ...)
}


#' Normal Distribution Density Plot with Area Shaded
#'
#' Plot of the Normal distribution density function, with typical defaults used
#' in introducing normal distributions, and an internal area shaded.
#'
#' \code{normal_area_plot_q} plots the density function and shades the area between \code{qmin}
#' and \code{qmax}. \code{normal_area_plot_p} plots the density function and shades
#' the area centered at the median such that \eqn{P(lb \leq X \leq ub) = p}.
#'
#' @param qmin Lower quantile of the shaded area
#' @param qmax Upper quantile of the shaded area
#' @param p Probabilities in the shaded area. The area is centered at the median
#'  of the distribution.
#' @param mean Location
#' @param sd Scale
#' @param max.sd The x-axis of the plot is \code{mean + c(-1, 1) * max.sd * sd }.
#' @param area_opts Additional arguments passed to \code{\link{geom_dnorm_area}}.
#' @param ... Arguments passed to \code{\link{normal_plot}}.
#'
#' @family Plots of the normal distribution
#' @rdname normal_area_plots
#' @export
normal_area_plot_q <- function(qmin, qmax, mean = 0, sd = 1, max.sd = 4,
                               area_opts = list(), ...) {
  limits <- mean + max.sd * c(-1, 1) * sd
  qmin <- max(qmin, limits[1])
  qmax <- min(qmax, limits[2])
  (normal_plot(mean = mean, sd = sd, max.sd = max.sd, ...)
   + do.call(geom_dnorm_area, c(list(qmin, qmax, mean = mean, sd = sd),
                                area_opts))
  )
}

#' @rdname normal_area_plots
#' @export
normal_area_plot_p <- function(p, mean = 0, sd = 1, ...) {
  q <- qnorm((1 - p) / 2, lower.tail = TRUE)
  normal_area_plot_q(qmin = -q * sd + mean, qmax = q * sd + mean,
                     mean = mean, sd = sd, ...)
}

########### Students t

#' Student's t Density Plot
#'
#' Plot of the Student's t distribution density function, with typical defaults used
#' in introducing t distributions.
#'
#' @param df Degrees of freedom
#' @param mean Location
#' @param scale Scale
#' @param max.scale The x-axis of the plot is
#' \code{mean + c(-1, 1) * max.scale * scale }.
#' @param n Number of points to interpolate along
#' @param digits Number of digits to display in x-axis breaks.
#' @param ... Arguments passed to \code{\link{geom_dt_line}}.
#'
#' @family Plots of the t distribution
#' @export
students_t_plot <- function(df, mean = 0, scale = 1, max.scale = 4, n = 101,
                            digits = 2, ...) {
  limits <- mean + max.scale * c(-1, 1) * scale
  x <- seq(limits[1], limits[2], length.out = n)
  breaks <- seq(mean - max.scale * scale,
                mean + max.scale * scale,
                by = scale)
  (ggplot()
   + geom_dt_line(xmin = limits[1],
                  xmax = limits[2],
                  df = df, mean = mean, scale = scale, ...)
   + scale_x_continuous("x", limits = limits,
                        breaks = round(breaks, 2))
   + scale_y_continuous("p(x)"))
}


#' Student's t Density Plot with Tail Areas
#'
#' Plot of the Student's t distribution density function, with typical defaults used
#' in introducing normal distributions, and tail areas shaded.
#'
#' @param q Quantile to use for the tails
#' @param p Probabilities of the tail areas
#' @param df Degrees of freedom
#' @param mean Location
#' @param scale Scale
#' @param lower.tail logical; if \code{TRUE}, probabilities are \eqn{P(X \leq x)},
#'   else probabilities are \eqn{P(X > x)}.
#' @param two.sided logical; if \code{TRUE}, two-sided tails. The tails are are
#'   at \code{c(-q, q)} if quantiles are used, and at the quantiles such that
#'   \eqn{P(X \leq x) = p} and \eqn{P(X > x)} if probabilities are used.
#' @param max.scale The x-axis of the plot is
#'   \code{mean + c(-1, 1) * max.scale * scale }.
#' @param area_opts list. Arguments passed to \code{\link{geom_dt_area}}
#' @param ... Arguments passed to \code{\link{geom_dt_line}}.
#'
#' @family Plots of the t distribution
#' @rdname students_t_tail_plots
#' @export
students_t_tail_plot_q <- function(q, df, mean = 0, scale = 1,
                                   lower.tail = TRUE, two.sided = FALSE,
                                   area_opts = list(),
                                   max.scale = 4,
                                   ...) {
  limits <- mean + max.scale * c(-1, 1) * scale

  gg <- students_t_plot(df = df, mean = mean, scale = scale, ...)
  if (! two.sided) {
    if (lower.tail) {
      if (q > limits[1]) {
        gg <- gg + do.call(geom_dt_area, c(list(xmin = limits[1],
                                                xmax = q, df = df,
                                                mean = mean, scale = scale),
                                           area_opts))
      }
    } else {
      if (q < limits[2]) {
        gg <- gg +  do.call(geom_dt_area, c(list(xmin = q,
                                                 xmax = limits[2],
                                                 df = df,
                                                 mean = mean,
                                                 scale = scale),
                                            area_opts))
      }
    }
  } else {
    q <- c(-1, 1) * abs((q - mean) / scale) * scale + mean
    if (q[1] > limits[1]) {
      gg <- gg + do.call(geom_dt_area, c(list(df = df, xmin = limits[1], xmax = q[1],
                                              mean = mean, scale = scale),
                                         area_opts))
    }
    if (q[2] < limits[2]) {
      gg <- gg + do.call(geom_dt_area,
                         c(list(df = df, xmin = q[2], xmax = limits[2],
                                mean = mean, scale = scale),
                           area_opts))
    }
  }
  gg
}

#' @rdname students_t_tail_plots
#' @export
students_t_tail_plot_p <- function(p, df, mean = 0, scale = 1,
                                   lower.tail = TRUE, two.sided = FALSE,
                                   ...) {
  if (! two.sided) {
    q <- qnorm(p, lower.tail = lower.tail) * scale + mean
  } else {
    q <- qnorm(p / 2, lower.tail = TRUE) * scale + mean
  }
  students_t_tail_plot_q(q, df = df, mean = mean, scale = scale,
                         lower.tail = lower.tail,
                         two.sided = two.sided, ...)
}

#' Student's t Distribution Density Plot with Area Shaded
#'
#' Plot of the Student's t distribution density function, with typical defaults used
#' in introducing Student's t distributions, and an internal area shaded.
#'
#' \code{students_t_area_plot_q} plots the density function and shades the area between \code{qmin}
#' and \code{qmax}. \code{students_t_area_plot_p} plots the density function and shades
#' the area centered at the median such that \eqn{P(lb \leq X \leq ub) = p}.
#'
#' @param qmin Lower quantile of the shaded area
#' @param qmax Upper quantile of the shaded area
#' @param p Probabilities in the shaded area. The area is centered at the median
#'  of the distribution.
#' @param df Degrees of freedom
#' @param mean Location
#' @param scale Scale
#' @param max.scale The x-axis of the plot is \code{mean + c(-1, 1) * max.sd * sd }.
#' @param area_opts Additional arguments passed to \code{\link{geom_dnorm_area}}.
#' @param ... Arguments passed to \code{\link{students_t_plot}}.
#'
#' @family Plots of the t distribution
#' @rdname students_t_area_plots
#' @export
students_t_area_plot_q <- function(qmin, qmax, df, mean = 0, scale = 1,
                                   max.scale = 4,
                                   area_opts = list(), ...) {
  limits <- mean + max.scale * c(-1, 1) * scale
  qmin <- max(qmin, limits[1])
  qmax <- min(qmax, limits[2])
  (students_t_plot(mean = mean, scale = scale, max.scale = max.scale, ...)
   + do.call(geom_dt_area, c(list(xmin = qmin, xmax = qmax,
                                  df = df, mean = mean, scale = scale),
                             area_opts))
  )
}

#' @rdname students_t_area_plots
#' @export
students_t_area_plot_p <- function(p, df, mean = 0, scale = 1, ...) {
  q <- qnorm((1 - p) / 2, lower.tail = TRUE)
  students_t_area_plot_q(-q * scale + mean, q * scale + mean,
                         mean = mean, scale = scale, ...)
}


##################

#' Plot probability ditribution
#'
#' @param dist Distribution name. Used for functions \code{pdist}, \code{ddist},
#'   \code{qdist}.
#' @param n Number of points to interpolate over
#' @param xmin Minimum value on the x-axis scale
#' @param xmax Maximum value on the x-axis scale
#' @param area numeric vector of length 2. Minimum and maximum values of an area
#'   under the curve to shade.
#' @param p Probability area to shade.
#' @param tails logical. Shade areas in the tails
#' @param line_opts list. Arguments passed to \code{\link{geom_line}}.
#' @param area_opts list. Arguments passed to \code{\link{geom_ribbon}}.
#' @param ... Arguments passed to the distribution functions.
#' @rdname plot_dist_density
#' @export
#' @examples
#' plot_dist_density("norm", -4, 4, area = c(-2, 2))
#' plot_dist_density("norm", -4, 4, area = c(-2, 2), tails = TRUE)
#' plot_dist_density_p("norm", -4, 4, p = 0.95)
#' plot_dist_density_p("norm", -4, 4, p = 0.95, tails = TRUE)
plot_dist_density <- function(dist, xmin = NULL, xmax = NULL,
                              area = NULL, tails = FALSE,
                              line_opts = list(), area_opts = list(),
                              n = 101, ...) {
  qfun <- match.fun(str_c("q", dist))
  pfun <- match.fun(str_c("p", dist))
  dfun <- match.fun(str_c("d", dist))

  if (is.null(xmin)) {
    xmin <- qfun(0.005, ...)
  }
  if (is.null(xmax)) {
    xmax <- qfun(0.995, ...)
  }
  line_data <- data_frame(x = seq(xmin, xmax, length.out = n))
  line_data$y <- dfun(line_data$x, ...)

  pp <- ggplot() +
         do.call(geom_line,
              c(list(data = line_data,
                     mapping = aes_string(x = "x", y = "y")),
                line_opts))

  if (!is.null(area)) {
    if (length(area) == 1) {
      area <- c(-1, 1) * area
    }
    area <- sort(area[1:2])
    if (! tails) {
      area_min <- max(area[1], xmin)
      area_max <- min(area[2], xmax)
      area_data <- data_frame(x = seq(area_min, area_max, length.out = n),
                              ymin = 0)
      area_data$ymax <- dfun(area_data$x, ...)
      pp <- pp + do.call(geom_ribbon,
                         c(list(data = area_data,
                                mapping = aes_string(x = "x", ymin = "ymin",
                                                     ymax = "ymax")),
                           area_opts))
    } else {
      area_min <- max(area[1], xmin)
      area_max <- min(area[2], xmax)
      if (area_min > xmin) {
        area1_data <- data_frame(x = seq(xmin, area_min, length.out = n),
                                ymin = 0)
        area1_data$ymax <- dfun(area1_data$x, ...)
        pp <- pp + do.call(geom_ribbon,
                           c(list(data = area1_data,
                                  mapping = aes_string(x = "x", ymin = "ymin",
                                                       ymax = "ymax")),
                             area_opts))
      }
      if (area_max < xmax) {
        area2_data <- data_frame(x = seq(area_max, xmax, length.out = n),
                                 ymin = 0)
        area2_data$ymax <- dfun(area2_data$x, ...)
        pp <- pp + do.call(geom_ribbon,
                           c(list(data = area2_data,
                                  mapping = aes_string(x = "x", ymin = "ymin",
                                                       ymax = "ymax")),
                             area_opts))
      }

    }
  }
  pp
}


#' @rdname plot_dist_density
#' @export
plot_dist_density_p <- function(dist, xmin = NULL, xmax = NULL,
                                p = NULL, tails = FALSE,
                                line_opts = list(),
                                area_opts = list(), n = 101, ...) {
  qfun <- match.fun(str_c("q", dist))
  if (! is.null(p)) {
    ptail <- (1 - p) / 2
    area <- c(qfun(ptail, ...), qfun(ptail, ..., lower.tail = FALSE))
  }
  plot_dist_density(dist, xmin = xmin, xmax = xmax, area = area,
                    line_opts = line_opts, area_opts = area_opts, n = n, ...)

}

