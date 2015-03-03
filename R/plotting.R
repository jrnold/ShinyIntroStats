geom_dnorm_area <- function(xmin, xmax, mean = 0, sd = 1, n = 101, ...) {
  .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                      ymin = 0,
                      ymax = dnorm(x, mean = mean, sd = sd))
  geom_ribbon(data = .data, mapping = aes(x = x, ymin = ymin, ymax = ymax),
              ...)
}

geom_dnorm_line <- function(mean = 0, sd = 1, n = 101, ...) {
  .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                      y = dt((x - mean) / scale, mean, sd))
  geom_line(data = .data, mapping = aes(x = x, y = y), ...)
}

geom_dt_line <- function(df, xmin, xmax, mean = 0, scale = 1, n = 101, ...) {
  if (is.infinte(df)) {
    geom_dnorm_line(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        y = dt((x - mean) / scale, mean, sd))
    geom_line(data = .data, mapping = aes(x = x, y = y), ...)
  }
}

geom_dt_area <- function(df, xmin, xmax, mean = 0, scale = 1,
                         ...) {
  if (is.infinite(dt)) {
    geom_dnorm_area(xmin, xmax, mean = mean, sd = scale, ...)
  } else {
    .data <- data_frame(x = seq(xmin, xmax, length.out = n),
                        ymin = 0,
                        ymax = dnorm(x, mean = mean, sd = sd))
    geom_ribbon(data = .data, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                ...)
  }
}

## Normal plots

normal_plot <- function(mean = 0, sd = 1, max.sd = 4, n = 1000, ...) {
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
   + do.call(geom_dnorm_area, c(list(lb, ub, mean = mean, sd = sd),
                                area_opts))
  )
}

normal_area_plot_p <- function(p, mean = 0, sd = 1, ...) {
  q <- qnorm((1 - p) / 2, lower.tail = TRUE)
  normal_area_plot_q(-q * sd + mean, q * sd + mean,
                     mean = mean, sd = sd, ...)
}

########### Students t

student_t_plot <- function(df, mean = 0, scale = 1, max.scale = 4, n = 1000, ...) {
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
                        breaks = breaks)
   + scale_y_continuous("p(x)"))
}


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
      gg <- gg + do.call(geom_dt_area, c(list(xmin = limits[1], xmax = q[1],
                                              df = df, mean = mean,
                                              scale = scale),
                                         area_opts))
    }
    if (q[2] < limits[2]) {
      gg <- gg + do.call(geom_dt_area,
                         c(list(xmin = q[2], xmax = limits[2],
                                mean = mean, scale = scale),
                           area_opts))
    }
  }
  gg
}

students_t_tail_plot_p <- function(p, mean = 0, scale = 1,
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

students_t_area_plot_q <- function(lb, ub, mean = 0, sd = 1, max.scale = 4,
                                   area_opts = list(), ...) {
  limits <- mean + max.scale * c(-1, 1) * scale
  lb <- max(lb, limits[1])
  ub <- min(ub, limits[2])
  (students_t_plot(mean = mean, sd = scale, max.scale = max.scale, ...)
   + do.call(geom_dt_area, c(list(xmin = lb, xmax = ub,
                                  df = df, mean = mean, scale = scale),
                             area_opts))
  )
}

students_t_area_plot_p <- function(p, mean = 0, sd = 1, ...) {
  q <- qnorm((1 - p) / 2, lower.tail = TRUE)
  students_t_plot_q(-q * scale + mean, q * scale + mean,
                    mean = mean, scale = scale, ...)
}
