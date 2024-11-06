test_that("can evaluate Normal quantiles", {
  n <- 10
  p <- seq(0, 1, length.out = n)
  m <- rnorm(n)
  logs <- rnorm(n)
  s <- exp(logs)
  NormalScaleTranslate <- Reduce(append_flow, list(Scale, Translate),
    init = Normal(0, 1))
  expect_equal(
    NormalScaleTranslate$quantile(p, list(logs, m)),
    qnorm(p, mean = m, sd = s)
  )
})

test_that("can update standard Normal loglikelihood", {
  n <- 10
  y <- rnorm(n)
  m <- rnorm(n)
  logs <- rnorm(n)
  s <- exp(logs)
  NormalScaleTranslate <- Reduce(append_flow, list(Scale, Translate),
    init = Normal(0, 1))
  expect_equal(
    NormalScaleTranslate$log_density(y, list(logs, m)),
    dnorm(y, mean = m, sd = s, log = TRUE)
  )
})

test_that("can update Bernoulli loglikelihood", {
  n <- 10
  y <- rbinom(n, size = 1, prob = 0.5)
  logscale0 <- -rlnorm(n)
  logscale1 <- -rlnorm(n)
  scale0 <- exp(logscale0)
  scale1 <- exp(logscale1)
  BernoulliScaleRisk0ScaleRisk1 <- Reduce(append_flow, 
    list(ScaleRisk0, ScaleRisk1), init = Bernoulli(0.5))
  expect_equal(
    BernoulliScaleRisk0ScaleRisk1$log_density(y,
      list(logscale0, logscale1)),
    dbinom(y, size = 1,
      prob = (1 - (1 - 0.5) * scale0) * scale1,
      log = TRUE)
  )
})

test_that("can evaluate Bernoulli scores", {
  n <- 10
  y <- rbinom(n, size = 1, prob = 0.5)
  logscale0 <- -rlnorm(n)
  logscale1 <- -rlnorm(n)
  scale0 <- exp(logscale0)
  scale1 <- exp(logscale1)
  BernoulliScaleRisk0 <- append_flow(Bernoulli(0), ScaleRisk0)
  BernoulliScaleRisk0ScaleRisk1 <-
    append_flow(BernoulliScaleRisk0, ScaleRisk1)
  expect_equal(Bernoulli(0)$grad_log_density(y, list()),
    list()) 
  expect_equal(BernoulliScaleRisk0$grad_probability(
      list(logscale0)),
    list(-exp(logscale0)))
  expect_equal(BernoulliScaleRisk0ScaleRisk1$grad_probability(
      list(logscale0, logscale1)),
    list(-exp(logscale0) * exp(logscale1),
      (1 - exp(logscale0)) * exp(logscale1)))
  expect_equal(BernoulliScaleRisk0ScaleRisk1$grad_log_density(y,
      list(logscale0, logscale1)),
    list(
      -exp(logscale0) * exp(logscale1) * 
        (y / ((1 - exp(logscale0)) * exp(logscale1)) -
          (1 - y) /
            (1 - (1 - exp(logscale0)) * exp(logscale1))),
      (1 - exp(logscale0)) * exp(logscale1) *
        (y / ((1 - exp(logscale0)) * exp(logscale1)) -
          (1 - y) /
            (1 - (1 - exp(logscale0)) * exp(logscale1)))))
})

test_that("can evaluate Normal scores", {
  n <- 10
  y <- rnorm(n)
  m <- rnorm(n)
  logs <- rnorm(n)
  s <- exp(logs)
  NormalScaleTranslate <- Reduce(append_flow, list(Scale, Translate),
    init = Normal())
  expect_equal(
    NormalScaleTranslate$deriv_log_density(y, list(logs, m)),
    (m - y) / (s^2) 
  )
  expect_equal(
    append_flow(Normal(), Translate)$grad_log_density(y, list(m)),
    list((y - m) / (1^2))
  )
  expect_equal(
    Normal()$deriv_log_density(Scale$data_tsfm$f_inv(y, logs), list()),
    -y / s
  )
  expect_equal(
    Scale$data_tsfm$grad_f_inv(y, logs),
    -y / s
  )
  expect_equal(
    Scale$data_tsfm$grad_log_deriv_f_inv(y, logs),
    rep(-1, length(y))
  )
  expect_equal(
    append_flow(Normal(), Scale)$grad_log_density(y, list(logs)),
    list(s^(-2) * (y - 0)^2 - 1)
  )
  expect_equal(
    append_flow(Normal(), Scale)$grad_log_density(y, list(logs)),
    list((y - 0)^2 / s^2 - 1)
  )
  expect_equal(
    NormalScaleTranslate$grad_log_density(y, list(logs, m)),
    list((y - m)^2 / s^2 - 1, (y - m) / s^2)
  )
})

test_that("can evaluate LogNormal scores", {
  n <- 10
  y <- rlnorm(n)
  logm <- rnorm(n)
  m <- exp(logm)
  logs <- rnorm(n)
  s <- exp(logs)
  LogNormalPower <- Reduce(append_flow, list(Power), init = LogNormal())
  LogNormalPowerScale <- Reduce(append_flow, list(Power, Scale),
    init = LogNormal())
  expect_equal(
    LogNormal()$deriv_log_density(y, list()),
    (0 - 1^2 - log(y)) / (1^2 * y) 
  )
  expect_equal(
    LogNormalPowerScale$deriv_log_density(y, list(logs, logm)),
    (logm - s^2 - log(y)) / (s^2 * y) 
  )
  expect_equal(
    LogNormalPower$grad_log_density(y, list(logs)),
    list((log(y))^2 / (s^2) - 1)
  )
  expect_equal(
    LogNormalPowerScale$grad_log_density(y, list(logs, logm)),
    list((log(y) - logm)^2 / (s^2) - 1,
      (log(y) - logm) / (s^2))
  )
})
