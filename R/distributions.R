#' Normal distribution as a CompositeFamily
#'
#' @param mean the mean
#' @param sd the standard deviation
#' @returns a new ContinuousFamily object
#' @examples
#' dist <- Normal()
#' dist$quantile(0.95)
#' @export
Normal <- function(mean = 0, sd = 1) {
  ContinuousFamily$new(
    name = paste0("N(", mean, ",", sd, ")"),
    flows = list(),
    log_density = function(y, parameters) {
      stats::dnorm(y, mean = mean, sd = sd, log = TRUE)
    },
    quantile = function(p, parameters) {
      stats::qnorm(p, mean = mean, sd = sd)
    },
    cdf = function(q, parameters) {
      stats::pnorm(q, mean = mean, sd = sd)
    },
    deriv_log_density = function(y, parameters) {
      (mean - y) / (sd^2)
    }
  )
}

#' Lognormal distribution as a CompositeFamily
#'
#' @param meanlog the mean of the logarithm
#' @param sdlog the standard deviation of the logarithm
#' @returns a new ContinuousFamily object
#' @examples
#' dist <- LogNormal()
#' log(dist$quantile(0.95))
#' @export
LogNormal <- function(meanlog = 0, sdlog = 1) {
  ContinuousFamily$new(
    name = paste0("logN(", meanlog, ",", sdlog, ")"),
    log_density = function(y, parameters) {
      stats::dlnorm(y, meanlog = meanlog, sdlog = sdlog, log = TRUE)
    },
    quantile = function(p, parameters) {
      stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog)
    },
    cdf = function(q, parameters) {
      stats::plnorm(q, meanlog = meanlog, sdlog = sdlog)
    },
    deriv_log_density = function(y, parameters) {
      (meanlog - sdlog^2 - log(y)) / (sdlog^2 * y)
    }
  )
}

#' Bernoulli distribution as a CompositeFamily
#'
#' @param prob the probability of a success
#' @returns a new BinaryFamily object
#' @examples
#' dist <- Bernoulli()
#' dist$probability()
#' @export
Bernoulli <- function(prob = 0.5) {
  BinaryFamily$new(
    name = paste0("Ber(", prob, ")"),
    probability = function(parameters) {
      prob
    }
  )
}
