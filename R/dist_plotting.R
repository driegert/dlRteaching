
#' Plotting Binomial PMFs
#'
#' Plots and colours Binomial pmfs according to x-values given
#'
#' @details Colouring not implemented, use \link{ggbinom_pmf} instead.
#'
#' @export
binom_pmf <- function(n, p, x_shaded = NULL, ...){
  opar <- par(no.readonly = TRUE)
  par(...)
  x <- 0:n
  px <- dbinom(x, size = n, prob = p)

  barplot(px, names.arg = x, main = paste0("PMF of Binomial R.V. with n = ", n, " and p = ", p))

  par(opar)
}

#'Plotting Binomial PMFs
#'
#' Plots and colours Binomial pmfs according to x-values given
#'
#' @export
ggbinom_pmf <- function(n, p, x_shaded = NULL, ...){
  invisible(require(ggplot2))
  theme_set(theme_bw())

  x <- 0:n
  px <- dbinom(x, size = n, prob = p)
  plt <- data.frame(x = x, px = px, shaded = rep("not shaded", n+1))

  if (!is.null(x_shaded)){
    plt[ x %in% x_shaded , "shaded"] <- "shaded"
  }

  plt$shaded <- factor(plt$shaded, levels = c("not shaded", "shaded"))

  ggplot(plt, aes(x = factor(x), y = px, fill = shaded)) +
    geom_col(colour = "black") +
    scale_fill_manual(values = c("grey90", "red3")) +
    scale_x_discrete(labels = plt$x) +
    theme(legend.position = "none") +
    labs(x = "x", y = "p(x)"
         , title = paste0("PMF of Binomial R.V. with n = ", n, " and p = ", p))
}

#' @export
ggnorm_pdf <- function(mean = 0, sd = 1, x_incr = 0.01, x_shaded = NULL, lower.tail = TRUE){
  invisible(require(ggplot2))
  theme_set(theme_bw())

  x <- seq(mean - 3.5*sd, mean+3.5*sd, by = x_incr)
  fx <- dnorm(x, mean = mean, sd = sd)

  plt <- data.frame(x, fx)

  plta <- data.frame(xg = c(x[1], x, tail(x,1))
                     , fxg = c(0, dnorm(x, mean = mean, sd = sd), 0))

  # gg1 <- ggplot(plt, aes(x = x, y = fx)) +
  #   geom_polygon(data = plta, mapping = aes(x = xg, y = fxg), fill = "grey")
  gg1 <- ggplot(plta, aes(x = xg, y = fxg)) + geom_polygon(fill = "grey90")

  if (!is.null(x_shaded)){
    if (length(x_shaded) == 1){
      x_shaded_end <- ifelse(lower.tail, mean - 4*sd, mean+4*sd)
      x_shade_mid <- seq(x_shaded, x_shaded_end
                         , by = ifelse(lower.tail, -x_incr, x_incr))
      xs <- c(x_shaded, x_shade_mid, x_shaded_end)
      fxs <- c(0, dnorm(x_shade_mid, mean = mean, sd = sd), 0)
    } else {
      x_shade_mid <- seq(x_shaded[1], x_shaded[2], by = x_incr)
      xs <- c(x_shaded[1], x_shade_mid, x_shaded[2])
      fxs <- c(0, dnorm(x_shade_mid, mean = mean, sd = sd), 0)
    }

    plt2 <- data.frame(xs = xs, fxs = fxs)

    gg1 <- gg1 + geom_polygon(data = plt2, aes(xs, fxs), fill = "red3")
  }

  gg1 <- gg1 + geom_line() +
    labs(x = "x", y = "f(x)"
         , title = bquote(paste("PDF of Normal R.V. with ", mu, " = ", .(mean)
                                , " and ", sigma, " = ", .(sd)))) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(mean-3*sd, mean+3*sd, by = sd))

  gg1
}


#' @export
ggchisq_pdf <- function(df, x_incr = 0.01, x_shaded = NULL
                        , lower.tail = TRUE){
  invisible(require(ggplot2))
  theme_set(theme_bw())

  sd <- sqrt(2*df)
  mean <- df
  x_tails <- c(max(0, df - 3*sd), max(10, df + 5*sd))
  x <- seq(x_tails[1], x_tails[2], by = x_incr)
  fx <- dchisq(x, df = df)

  plt <- data.frame(x, fx)

  plta <- data.frame(xg = c(x[1], x, tail(x,1))
                     , fxg = c(0, dchisq(x, df = df), 0))

  # gg1 <- ggplot(plt, aes(x = x, y = fx)) +
  #   geom_polygon(data = plta, mapping = aes(x = xg, y = fxg), fill = "grey")
  gg1 <- ggplot(plta, aes(x = xg, y = fxg)) + geom_polygon(fill = "grey90")

  if (!is.null(x_shaded)){
    if (length(x_shaded) == 1){
      x_shaded_end <- ifelse(lower.tail, max(0, mean - 4*sd), mean+5*sd)
      x_shade_mid <- seq(x_shaded, x_shaded_end
                         , by = ifelse(lower.tail, -x_incr, x_incr))
      xs <- c(x_shaded, x_shade_mid, x_shaded_end)
      fxs <- c(0, dchisq(x_shade_mid, df = df), 0)
    } else {
      x_shade_mid <- seq(x_shaded[1], x_shaded[2], by = x_incr)
      xs <- c(x_shaded[1], x_shade_mid, x_shaded[2])
      fxs <- c(0, dchisq(x_shade_mid, df = df), 0)
    }

    plt2 <- data.frame(xs = xs, fxs = fxs)

    gg1 <- gg1 + geom_polygon(data = plt2, aes(xs, fxs), fill = "red3")
  }

  gg1 <- gg1 + geom_line() +
    labs(x = "x", y = "f(x)"
         , title = bquote(paste("PDF of ", chi^2, " R.V. with ", lambda, " = ", .(df)))) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none") # +
    # scale_x_continuous(breaks = seq(x_tails[1], x_tails[2] - 0.5*sd, by = sd))

  gg1
}
