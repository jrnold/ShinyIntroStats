library("dplyr")
library("ggplot2")

geom_normal_polygon <- function(xmin, xmax, mean = 0, sd = 1, n = 100, ...) {
  .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                      ymin = 0,
                      ymax = dnorm(x, mean = mean, sd = sd))
  geom_ribbon(data = .data, mapping = aes(x = x, ymin = ymin, ymax = ymax),
              ...)
}

geom_normal_line <- function(xmin, xmax, mean = 0, sd = 1, n = 100, ...) {
  .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                      y = dnorm(x, mean, sd))
  geom_line(data = .data, mapping = aes(x = x, y = y), ...)
}

geom_students_t_line <- function(xmin, xmax, df = Inf, mean = 0, scale = 1, ...) {
  if (df == Inf) {
    geom_normal_line(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        y = dt((x - mean) / scale, mean, sd))
    geom_line(data = .data, mapping = aes(x = x, y = y), ...)
  }
}

geom_students_t_polygon <- function(xmin, xmax, df = Inf, mean = 0, scale = 1,
                                    ...) {
  if (df == Inf) {
    geom_normal_polygon(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        ymin = 0,
                        ymax = dnorm(x, mean = mean, sd = sd))
    geom_ribbon(data = .data, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                ...)
  }
}

normal_plot <- function(mean, sd, max.sd = 4, n = 1000, ...) {
  limits <- mean + max.sd * c(-1, 1) * sd
  x <- seq(limits[1], limits[2], length.out = n)
  breaks <- seq(mean - max.sd * sd,
                mean + max.sd * sd,
                by = sd)

  (ggplot()
   + geom_normal_line(xmin = limits[1],
                      xmax = limits[2],
                      mean = mean, sd = sd, ...)
   + scale_x_continuous("x", limits = limits,
                        breaks = breaks)
   + scale_y_continuous("p(x)"))
}


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
        gg <- gg + do.call(geom_normal_polygon, c(list(limits[1], q, mean, sd),
                                                  area_opts))
      }
    } else {
      if (q < limits[2]) {
        gg <- gg +  do.call(geom_normal_polygon, c(list(q, limits[2], mean, sd),
                                                   area_opts))
      }
    }
  } else {
    q <- c(-1, 1) * abs(q)
    if (q[1] > limits[1]) {
      gg <- gg + do.call(geom_normal_polygon, c(list(limits[1], q[1], mean, sd),
                                                area_opts))
    }
    if (q[2] < limits[2]) {
      gg <- gg + do.call(geom_normal_polygon, c(list(q[2], limits[2], mean, sd),
                                                area_opts))
    }
  }
  gg
}

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

normal_area_plot_q <- function(lb, ub, mean = 0, sd = 1, max.sd = 4,
                               area_opts = list(), ...) {
  limits <- mean + max.sd * c(-1, 1) * sd
  lb <- max(lb, limits[1])
  ub <- min(ub, limits[2])
  (normal_plot(mean = mean, sd = sd, max.sd = max.sd, ...)
   + do.call(geom_normal_polygon, c(list(lb, ub, mean = mean, sd = sd),
                            area_opts))
  )
}

normal_area_plot_p <- function(p, mean = 0, sd = 1, ...) {
  q <- qnorm((1 - p) / 2, mean, sd, lower.tail = TRUE)
  normal_area_plot_q(-q, q, mean = mean, sd = sd, ...)
}

